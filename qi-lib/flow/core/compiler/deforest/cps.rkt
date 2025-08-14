#lang racket/base

(provide (for-syntax deforest-pass))

(require (for-syntax racket/base
                     syntax/parse
                     "syntax.rkt"
                     "../../../extended/util.rkt"
                     syntax/srcloc
                     racket/syntax-srcloc
                     "fusion.rkt"
                     "../../private/form-property.rkt"
                     "../../../../flow/aux-syntax.rkt")
         "templates.rkt"
         racket/performance-hint
         racket/match
         racket/contract/base)

;; "Composes" higher-order functions inline by directly applying them
;; to the result of each subsequent application, with the last argument
;; being passed to the penultimate application as a (single) argument.
;; This is specialized to our implementation of stream fusion in the
;; arguments it expects and how it uses them.
(define-syntax inline-compose1
  (syntax-rules ()
    [(_ f) f]
    [(_ [op (f ...) g ...] rest ...) (op f ... (inline-compose1 rest ...) g ...)]
    ))

;; Adds the initial states of all stateful transformers in the
;; required order to the initial producer state. Uses (cons Tx S)
;; where Tx is the transformer's initial state and S is the producer's
;; initial state with all preceding transformer states already
;; added. Nothing is added for stateless transformers which pass () as
;; their initial state expression. For example: (inline-consing (T1)
;; () (T2) P) -> (cons T2 (cons T1 P))
(define-syntax inline-consing
  (syntax-rules ()
    [(_ state () rest ...) (inline-consing state rest ...)]
    [(_ state (arg) rest ...) (inline-consing (cons arg state) rest ...)]
    [(_ state) state]
    ))

(begin-for-syntax

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Producers

  (define-syntax-class fsp
    #:attributes (name contract prepare next)
    (pattern range:fsp-range
             #:do ((define is (syntax-local-value #'range.info)))
             #:attr name #''range
             #:attr contract #'()
             #:attr prepare (apply (deforestable-info-prepare is) (syntax->list #'range.es^))
             #:do ((define is (syntax-local-value #'range.info)))
             #:when (and (deforestable-info? is)
                         (eq? (deforestable-info-kind is) 'P))
             #:attr next (deforestable-info-runtime is))
    (pattern default:fsp-default
             #:attr name #''list->cstream
             #:attr contract #'(list?)
             #:attr prepare #`(lambda (consing next)
                                 (lambda (lst)
                                   (next (consing lst))))
             #:attr next #'list->cstream-next
             #:attr state #'default.state))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Consumers

  (define-syntax-class fsc
    #:attributes (end)
    (pattern foldr:fsc-foldr
             #:attr end #'(foldr-cstream-next foldr.op foldr.init))
    (pattern foldl:fsc-foldl
             #:attr end #'(foldl-cstream-next foldl.op foldl.init))
    (pattern list-ref:fsc-list-ref
             #:attr end #'(list-ref-cstream-next list-ref.pos 'list-ref.name))
    (pattern length:fsc-length
             #:attr end #'(length-cstream-next))
    (pattern empty?:fsc-empty?
             #:attr end #'(empty?-cstream-next))
    (pattern default:fsc-default
             #:attr end #'(cstream-next->list))
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The pass

  ;; Performs deforestation rewrite on the whole syntax tree.
  (define-and-register-deforest-pass (deforest-pass ops ctx)
    (syntax-parse (reverse ops)
      [(c:fsc
        t:fst-new ...
        p:fsp)
       ;; A static runtime contract is placed at the beginning of the
       ;; fused sequence. And runtime checks for consumers are in
       ;; their respective implementation procedure.
       (with-syntax (((rt ...) (reverse (attribute t.state))))
         (attach-form-property
          #`(esc
             (contract (-> #,@#'p.contract any)
                       (p.prepare
                        (lambda (state)
                          (inline-consing state rt ...))
                        (#,@#'c.end
                         (inline-compose1 [t.next t.f
                                                  '#,(prettify-flow-syntax ctx)
                                                  '#,(build-source-location-vector
                                                      (syntax-srcloc ctx))
                                                  ] ...
                                          p.next
                                          )
                         '#,(prettify-flow-syntax ctx)
                         '#,(build-source-location-vector
                             (syntax-srcloc ctx))))
                       p.name
                       '#,(prettify-flow-syntax ctx)
                       #f
                       '#,(build-source-location-vector
                           (syntax-srcloc ctx))))))]))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime

(begin-encourage-inline

  ;; Producers

  (define-inline (list->cstream-next done skip yield)
    (λ (state)
      (cond [(null? state) (done)]
            [else (yield (car state) (cdr state))])))

  ;; Consumers

  (define-inline (cstream-next->list next ctx src)
    (λ (state)
      (let loop ([state state])
        ((next (λ () null)
               (λ (state) (loop state))
               (λ (value state)
                 (cons value (loop state))))
         state))))

  (define-inline (foldr-cstream-next op init next ctx src)
    (λ (state)
      (let loop ([state state])
        ((next (λ () init)
               (λ (state) (loop state))
               (λ (value state)
                 (op value (loop state))))
         state))))

  (define-inline (foldl-cstream-next op init next ctx src)
    (λ (state)
      (let loop ([acc init] [state state])
        ((next (λ () acc)
               (λ (state) (loop acc state))
               (λ (value state)
                 (loop (op value acc) state)))
         state))))

  (define-inline (list-ref-cstream-next init-countdown name next ctx src)
    (λ (state)
      (let loop ([state state]
                 [countdown init-countdown])
        ((next (λ () ((contract (-> pair? any)
                                (λ (v) v)
                                name ctx #f
                                src)
                      '()))
               (λ (state) (loop state countdown))
               (λ (value state)
                 (if (zero? countdown)
                     value
                     (loop state (sub1 countdown)))))
         state))))

  (define-inline (length-cstream-next next ctx src)
    (λ (state)
      (let loop ([state state]
                 [the-length 0])
        ((next (λ () the-length)
               (λ (state) (loop state the-length))
               (λ (value state)
                 (loop state (add1 the-length))))
         state))))

  (define-inline (empty?-cstream-next next ctx src)
    (λ (state)
      (let loop ([state state])
        ((next (λ () #t)
               (λ (state) (loop state))
               (λ (value state) #f))
         state))))

  )
