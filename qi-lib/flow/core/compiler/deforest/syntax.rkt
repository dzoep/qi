#lang racket/base

(provide fst-new
         fsp-new
         fsc-new)

(require syntax/parse
         "../../passes.rkt"
         "../../strategy.rkt"
         "../../../aux-syntax.rkt"
         (for-template racket/base
                       "../../passes.rkt"
                       "../../strategy.rkt")
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

;; TODO: this procedure is copied from 1000-qi0.rkt, it should
;; probably be moved to a supporting module for both the fallback
;; codegen and this syntax class.
(define (deforestable-clause-parser c)
  (syntax-parse c
    [((~datum floe) e) #'(qi0->racket e)]
    [((~datum expr) e) #'e]))

(define-syntax-class fsp-new
  #:attributes (contract prepare next name)
  #:literal-sets (fs-literals)
  (pattern (#%deforestable _name _info c ...)
           #:do ((define is (syntax-local-value #'_info)))
           #:when (and (deforestable-info? is)
                       (eq? (deforestable-info-kind is) 'P))
           #:with es^ #`#,(map deforestable-clause-parser (attribute c))
           #:attr contract #`(#,@(deforestable-info-rtacontract is))
           #:attr prepare (apply (deforestable-info-prepare is)
                                 (syntax->list #'es^))
           #:attr next (deforestable-info-runtime is)
           #:attr name #''name
           ))

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
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fusable Stream Consumers
;;
;; Syntax classes used for matching functions that can consume all
;; values from a sequence and create a single value from those.
;;
;; Prefixed with fsc- for clarity.

(define-syntax-class fsa
  #:attributes (expr)
  (pattern ((~datum floe) f-uncompiled)
           #:attr expr #`#,(run-passes #'f-uncompiled))
  (pattern ((~datum expr) expr)))

(define-syntax-class fsc-new
  #:attributes (end)
  #:literal-sets (fs-literals)
  (pattern (#%deforestable name _info arg:fsa ...)
           #:do ((define is (syntax-local-value #'_info)))
           #:when (and (deforestable-info? is)
                       (eq? (deforestable-info-kind is) 'C))
           #:with next (deforestable-info-runtime is)
           #:attr end #'(next arg.expr ...)))
