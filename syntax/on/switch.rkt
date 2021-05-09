#lang racket/base

(require syntax/parse/define
         racket/stxparam
         mischief/shorthand
         (only-in racket/function
                  const)
         (for-syntax racket/base)
         "base.rkt"
         "on.rkt")

(provide switch
         switch-lambda
         define-switch
         λ01
         <result>)

(define-syntax-parser switch-predicate
  [(_ (~datum else) arity:number)
   #'(const (void))]
  [(_ predicate:expr arity:number)
   #'(on-clause predicate arity)])

(define-syntax-parser switch-consequent
  [(_ ((~datum call) expr:clause) arg:expr ...)
   #'(on (arg ...) expr)]
  [(_ ((~datum connect) expr:expr ...) arg:expr ...)
   #'(switch (arg ...) expr ...)]
  [(_ consequent:expr arg:expr ...)
   #'consequent])

(define-syntax-parameter <result>
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside `on`")))

(define-syntax-parser switch
  [(_ args:subject
      [predicate:clause consequent ...]
      ...)
   #:do [(define arity (attribute args.arity))]
   #`(cond [((switch-predicate predicate #,arity) #,@(syntax->list (attribute args.args)))
            =>
            (λ (x)
              (syntax-parameterize ([<result> (make-rename-transformer #'x)])
                (switch-consequent consequent #,@(syntax->list (attribute args.args)))
                ...))]
           ...)])

(define-syntax-parser switch-lambda
  [(_ (arg:id ...) expr:expr ...)
   #'(lambda (arg ...)
       (switch (arg ...)
               expr ...))]
  [(_ rest-args:id expr:expr ...)
   #'(lambda rest-args
       (switch (rest-args)
               expr ...))])

(define-alias λ01 switch-lambda)

(define-syntax-parser define-switch
  [(_ (name:id arg:id ...) expr:expr ...)
   #'(define name
       (switch-lambda (arg ...)
                      expr ...))])