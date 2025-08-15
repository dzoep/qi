#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special syntax for templates

;; These bindings are used for ~literal matching to introduce implicit
;; producer/consumer when none is explicitly given in the flow.
(provide cstream->list)
(define cstream->list #'-cstream->list)
