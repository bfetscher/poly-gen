#lang racket

(require racket/cmdline
         redex/reduction-semantics
         "poly-lam.rkt"
         "unparse.rkt")

(define Mod-Template-Name "ModSkeleton.hs")
(define term-depth (make-parameter 5))
(define Output-Filename "TestModule.hs")

(define (gen-module num-exps)
  (define exp-text (gen-exp-text num-exps))
  (string-replace (mod-template-text)
                  "<<explist>>"
                  exp-text))

(define (gen-exp-text num-exps)
  (apply string-append
         (cons "\n  "
               (add-between
                (for/list ([_ (in-range num-exps)])
                  (unp-exp (one-term)))
                ",\n  "))))
  
(define (one-term)
  (display ".")
  (flush-output)
  (match (generate-term poly-stlc
                        #:satisfying
                        (typeof • M ((list int) → (list int)))
                        (term-depth))
    [#f (one-term)]
    [`(typeof • ,M ((list int) → (list int))) M]))

(define (mod-template-text)
  (call-with-input-file Mod-Template-Name
    (λ (in)
      (port->string in))))

(command-line #:args (num-exps)
              (call-with-output-file Output-Filename
                (λ (out)
                  (write-string (gen-module (string->number num-exps))
                                out))
                #:exists 'replace))