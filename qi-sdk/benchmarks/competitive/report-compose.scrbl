#lang scribble/manual

@require[scribble-math/dollar
	 srfi/19
	 vlibench
	 (for-syntax racket/base)
	 racket/cmdline
	 racket/string
	 racket/function]

@;Command-line handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(define config-profile
  (let ()
    (define profile-box (box 'preview))
    (command-line
      #:once-each
      (("-p" "--profile")
       name
       "profile name to use (use 'list' to list available profiles)"
       (when (equal? name "list")
	 (displayln
	  (format
	   "Available profiles: ~a"
	   (string-join
	    (for/list (((k v) vlib/profiles))
	      (symbol->string k))
	    ", ")))
	 (exit 0))
       (set-box! profile-box (string->symbol name))))
    (unbox profile-box)))

@title[#:style (with-html5 manual-doc-style)]{Qi Normal/Deforested Competitive Benchmarks}

@;Qi version helper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@(begin-for-syntax
  (require setup/getinfo)
  (define (get-version)
    ((get-info '("qi")) 'version)))
@(define-syntax (get-qi-version stx)
  (datum->syntax stx (get-version) stx stx))

@;Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@(module qi-compose racket/base
  (require qi
           racket/math
           racket/list
           vlibench
	   (for-syntax racket/base
	   	       syntax/parse
		       qi/flow/aux-syntax)
	   syntax/parse/define
	   qi/flow/core/passes)

(define-syntax-parser compose-with-values
    [(_ args)
     #'(apply values args)]
    [(_ args f g ...)
     #'(call-with-values
        (λ ()
          (compose-with-values args g ...))
        f)])
	
(begin-for-syntax

  (define (thread-parser stx)
    (syntax-parse stx
    #:datum-literals (#%host-expression)
      [(_ (esc (#%host-expression f)) ...)
       #:with (reversed-f ...) (reverse
                                (syntax->list
                                 #'(f ...)))
       #'(λ args
           (compose-with-values args
                                reversed-f ...))]))

  (define-and-register-pass 998 (compose-threading-form2 stx)
    (syntax-parse stx
      [((~or* (~datum ~>) (~datum thread)) onex:clause ...)
       #`(esc #,(thread-parser #'(~> onex ...)))]
      [else stx]))

  )

  (provide qi-thread/cp)

  (define impl-label "call-with-values")

  (define qi-thread/cp
    (make-vlib/prog impl-label
    		    (flow (~> add1 sqr sub1))))

  )

@(module qi-callwv racket/base
  (require qi
           qi/list
           racket/math
           vlibench)

  (provide qi-thread/wv)

  (define impl-label "compose")

  (define qi-thread/wv
    (make-vlib/prog impl-label
    		    (flow (~> add1 sqr sub1))))

  )

@(require 'qi-compose
          'qi-callwv)

@(define benchmarks-specs
  (list
   (vlib/spec 'thread
          identity
	  (list qi-thread/cp
	  	qi-thread/wv))))

@; Processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@define[profile (hash-ref vlib/profiles config-profile)]
@(define results
  (for/list ((spec (in-list benchmarks-specs)))
    (run-benchmark
      spec
      #:profile profile)))

@; Rendering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{General Information}

Date and time: @(date->string (seconds->date (current-seconds)))

@snippet:vlib/profile[profile]

@snippet:system-information[#:more-versions `(("Qi Version: " ,(get-qi-version)))]


@section{Summary Results}

@snippet:summary-results-table[results]


@section{Detailed Results}

@snippet:benchmark/s-duration[results]

Measured lengths: @(racket #,(for/list ((len (vlib/profile->steps profile))) len))

@(for/list ((result (in-list results)))
  (snippet:benchmark-result result))
