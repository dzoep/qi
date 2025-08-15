#lang racket/base

(provide (for-space qi
                    (except-out (all-defined-out)
                                range2
                                range)
                    (rename-out [range2 range])
                    (rename-out [list-tail drop])))

(require (for-syntax racket/base
                     "private/util.rkt")
         syntax/parse/define
         "flow/extended/expander.rkt"
         (only-in "flow/space.rkt"
                  define-qi-alias)
         "macro.rkt"
         (prefix-in r: racket/base)
         (prefix-in r: racket/list)
         racket/contract/base
         racket/match)

;; Transformers

(define-deforestable #:transformer (map [floe f])
  #'(lambda (vs)  ; single list arg
      (r:map f vs))
  (lambda (f next ctx src)
    (λ (done skip yield)
      (next done
            skip
            (λ (value state)
              (yield (f value) state))))))

(define-deforestable #:transformer (filter [floe f])
  #'(λ (vs)
      (r:filter f vs))
  (lambda (f next ctx src)
    (λ (done skip yield)
      (next done
            skip
            (λ (value state)
              (if (f value)
                  (yield value state)
                  (skip state)))))))

(define-deforestable #:transformer (filter-map [floe f])
  #'(λ (vs)
      (r:filter-map f vs))
  (lambda (f next ctx src)
    (λ (done skip yield)
      (next done
            skip
            (λ (value state)
              (let ([fv (f value)])
                (if fv
                    (yield fv state)
                    (skip state))))))))

(define-deforestable #:transformer (take [expr n])
  #'(λ (vs)
      (r:take vs n))
  (lambda (next ctx src)
    (λ (done skip yield)
      (λ (take-state)
        (define n (car take-state))
        (define state (cdr take-state))
        (if (zero? n)
            (done)
            ((next (λ ()
                     ((contract (-> pair? any)
                                (λ (v) v)
                                'take ctx
                                #f
                                src)
                      '()))
                   (λ (state)
                     (skip (cons n state)))
                   (λ (value state)
                     (define new-state (cons (sub1 n) state))
                     (yield value new-state)))
             state))))))

(define-deforestable #:transformer (filter-not [floe f])
  #'(lambda (vs)
      (r:filter-not f vs))
  (lambda (f next ctx src)
    (λ (done skip yield)
      (next done
            skip
            (λ (value state)
              (if (f value)
                  (skip state)
                  (yield value state)))))))

(define-deforestable #:transformer (list-tail [expr n])
  #'(lambda (vs)
      (r:list-tail vs n))
  (lambda (next ctx src)
    (λ (done skip yield)
      (λ (drop-state)
        (define n (car drop-state))
        (define state (cdr drop-state))
        (if (zero? n)
            ((next done
                   (λ (state)
                     (skip (cons n state)))
                   (λ (value state)
                     (define new-state (cons n state))
                     (yield value new-state)))
             state)
            ((next (λ ()
                     ((contract (-> pair? any)
                                (λ (v) v)
                                'list-tail ctx
                                #f
                                src)
                      '()))
                   (λ (state)
                     (skip (cons n state)))
                   (λ (value state)
                     (skip (cons (sub1 n) state))))
             state))))))

(define-qi-syntax-parser rest
  [_:id #'(list-tail 1)])

(define-qi-syntax-parser cdr
  [_:id #'(list-tail 1)])

(define-qi-syntax-parser cddr
  [_:id #'(list-tail 2)])

(define-qi-syntax-parser cdddr
  [_:id #'(list-tail 3)])

(define-qi-syntax-parser cddddr
  [_:id #'(list-tail 4)])

(define-qi-syntax-parser cdddddr
  [_:id #'(list-tail 5)])

;; Producers

(define-deforestable #:producer (range [expr low] [expr high] [expr step])
  #'(λ ()
      (r:range low high step))
  (lambda (done skip yield)
    (λ (state)
      (match-define (list l h s) state)
      (cond [(< l h)
             (yield l (cons (+ l s) (cdr state)))]
            [else (done)])))
  #'(lambda (consing next)
      (lambda ()
        (next (consing (list low high step)))))
  ())

(define-deforestable #:producer (list->cstream)
  #'identity
  (lambda (done skip yield)
    (λ (state)
      (cond [(null? state) (done)]
            [else (yield (car state) (cdr state))])))
  #'(lambda (consing next)
      (lambda (lst)
        (next (consing lst))))
  (list?))

;; We'd like to indicate multiple surface variants for `range` that
;; expand to a canonical form, and provide a single codegen just for the
;; canonical form.
;; Since `define-deforestable` doesn't support indicating multiple cases
;; yet, we use the ordinary macro machinery to expand surface variants of
;; `range` to a canonical form that is defined using
;; `define-deforestable`.
(define-qi-syntax-parser range2
  [(_ low:expr high:expr step:expr) #'(range low high step)]
  [(_ low:expr high:expr) #'(range low high 1)]
  [(_ high:expr) #'(range 0 high 1)]
  ;; not strictly necessary but this provides a better error
  ;; message than simply "range: bad syntax" that's warranted
  ;; to differentiate from racket/list's `range`
  [_:id (report-syntax-error this-syntax
          "(range arg ...)"
          "range expects at least one argument")])

;;

(define-deforestable (foldl [floe f] [expr init])
  #'(λ (vs)
      (r:foldl f init vs)))

(define-deforestable (foldr [floe f] [expr init])
  #'(λ (vs)
      (r:foldr f init vs)))

(define-deforestable car
  #'r:car)

(define-deforestable cadr
  #'r:cadr)

(define-deforestable caddr
  #'r:caddr)

(define-deforestable cadddr
  #'r:cadddr)

(define-deforestable (list-ref [expr n])
  #'(λ (vs)
      (r:list-ref vs n)))

(define-deforestable length
  #'r:length)

(define-deforestable empty?
  #'r:empty?)

(define-qi-alias null? empty?)
