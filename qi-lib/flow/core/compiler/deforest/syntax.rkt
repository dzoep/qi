#lang racket/base

(provide fst-new

         fsp-syntax
         fsc-syntax

         fsp-range
         fsp-default


         fsc-foldr
         fsc-foldl
         fsc-list-ref
         fsc-length
         fsc-empty?
         fsc-default

         )

(require syntax/parse
         "../../passes.rkt"
         "../../strategy.rkt"
         "../../../aux-syntax.rkt"
         (for-template racket/base
                       "../../passes.rkt"
                       "../../strategy.rkt"
                       "templates.rkt")
         (for-syntax racket/base
                     syntax/parse))

;; Literals set used for matching Fusable Stream Literals
(define-literal-set fs-literals
  #:datum-literals (esc #%host-expression #%fine-template #%blanket-template #%deforestable _ __)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fusable Stream Producers
;;
;; Syntax classes used for matching functions that produce a sequence
;; of values and they annotate the syntax with attributes that will be
;; used in the compiler to apply optimizations.
;;
;; All are prefixed with fsp- for clarity.

(define-syntax-class fsp-range
  #:attributes (arg state)
  #:literal-sets (fs-literals)
  #:datum-literals (range)
  (pattern (#%deforestable range _info ((~datum expr) the-arg) ...)
           #:attr state #'((list the-arg ...))
           #:attr arg #'(the-arg ...)))

(define-syntax-class fsp-default
  #:datum-literals (list->cstream)
  #:attributes (state)
  (pattern list->cstream
           #:attr contract #'(-> list? any)
           #:attr name #''list->cstream
           #:attr state #'()))

(define-syntax-class fsp-syntax
  (pattern (~or _:fsp-range
                _:fsp-default)))

(define-syntax-class fsp-new
  #:attributes (curry contract prepare next name)
  #:literal-sets (fs-literals)
  (pattern (#%deforestable name _info ((~datum expr) the-arg) ...)
           #:do ((define is (syntax-local-value #'_info)))
           #:when (and (deforestable-info? is)
                       (eq? (deforestable-info-kind is) 'P))
           #:attr curry (lambda () #'(lambda (proc) proc))
           #:attr contract #'(-> list? any)
           #:attr prepare #f
           #:attr next #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fusable Stream Transformers
;;
;; Syntax class matching all transformers defined by
;; (define-deforestable #:transformer ...)
;;
;; It provides the `next`, `f`, and `state` attributes as needed by
;; the deforest pass (see cps.rkt).
;;
;; It also allows `make-deforest-rewrire` to match it directly.
;;

(define-syntax-class fst-new
  #:attributes (next f state)
  #:literal-sets (fs-literals)
  (pattern (#%deforestable name _info ((~datum floe) f-uncompiled))
           #:do ((define is (syntax-local-value #'_info)))
           #:when (and (deforestable-info? is)
                       (eq? (deforestable-info-kind is) 'T))
           #:attr next (deforestable-info-runtime is)
           #:attr state #'()
           #:attr f #`(#,(run-passes #'f-uncompiled))
           )
  (pattern (#%deforestable name _info ((~datum expr) n))
           #:do ((define is (syntax-local-value #'_info)))
           #:when (and (deforestable-info? is)
                       (eq? (deforestable-info-kind is) 'T))
           #:attr next (deforestable-info-runtime is)
           #:attr state #'(n)
           #:attr f #'()
           )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fusable Stream Consumers
;;
;; Syntax classes used for matching functions that can consume all
;; values from a sequence and create a single value from those.
;;
;; Prefixed with fsc- for clarity.

(define-syntax-class fsc-foldr
  #:attributes (op init)
  #:literal-sets (fs-literals)
  #:datum-literals (foldr)
  (pattern (#%deforestable
            foldr
            _info
            ((~datum floe) op-uncompiled)
            ((~datum expr) init))
    #:attr op (run-passes #'op-uncompiled)))

(define-syntax-class fsc-foldl
  #:attributes (op init)
  #:literal-sets (fs-literals)
  #:datum-literals (foldl)
  (pattern (#%deforestable
            foldl
            _info
            ((~datum floe) op-uncompiled)
            ((~datum expr) init))
    #:attr op (run-passes #'op-uncompiled)))

(define-syntax-class cad*r-datum
  #:attributes (countdown)
  #:datum-literals (#%deforestable car cadr caddr cadddr)
  (pattern (#%deforestable car _info) #:attr countdown #'0)
  (pattern (#%deforestable cadr _info) #:attr countdown #'1)
  (pattern (#%deforestable caddr _info) #:attr countdown #'2)
  (pattern (#%deforestable cadddr _info) #:attr countdown #'3))

(define-syntax-class fsc-list-ref
  #:attributes (pos name)
  #:literal-sets (fs-literals)
  #:datum-literals (list-ref)
  ;; TODO: need #%host-expression wrapping idx?
  (pattern (#%deforestable list-ref _info ((~datum expr) idx))
    #:attr pos #'idx
    #:attr name #'list-ref)
  ;; TODO: bring wrapping #%deforestable out here?
  (pattern cad*r:cad*r-datum
    #:attr pos #'cad*r.countdown
    #:attr name #'cad*r))

(define-syntax-class fsc-length
  #:literal-sets (fs-literals)
  #:datum-literals (length)
  (pattern (#%deforestable length _info)))

(define-syntax-class fsc-empty?
  #:literal-sets (fs-literals)
  #:datum-literals (empty?) ; note: null? expands to empty?
  (pattern (#%deforestable empty? _info)))

(define-syntax-class fsc-default
  #:datum-literals (cstream->list)
  (pattern cstream->list))

(define-syntax-class fsc-syntax
  (pattern (~or _:fsc-foldr
                _:fsc-foldl
                _:fsc-list-ref
                _:fsc-length
                _:fsc-empty?
                _:fsc-default
                )))
