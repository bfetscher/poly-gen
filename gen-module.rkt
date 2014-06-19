#lang racket

(require racket/cmdline
         racket/runtime-path
         redex/reduction-semantics
         "poly-lam.rkt"
         "unparse.rkt")

(define-runtime-path Mod-Template "ModSkeleton.hs")
(define term-depth (make-parameter 5))
(define Output-Filename "TestModule.hs")
(define Term-Sizes-Filename "termsizes.txt")

(define (gen-module num-exps)
  (define exp-text (gen-exp-text num-exps))
  (string-replace (mod-template-text)
                  "<<explist>>"
                  exp-text))

(define (gen-exp-text num-exps)
  (define terms (for/list ([_ (in-range num-exps)])
                  (one-term)))
  (write-term-sizes terms)
  (apply string-append
         (cons "\n  "
               (add-between
                (map unp-exp terms)
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
  (call-with-input-file Mod-Template
    (λ (in)
      (port->string in))))

(define (write-term-sizes terms)
  (call-with-output-file Term-Sizes-Filename
    (λ (out)
      (void
       (write-string 
        (apply string-append
               (append
                (add-between
                 (map number->string
                      (map term-size terms))
                 " ")
                '("\n")))
        out)))
    #:exists 'replace))

(define (term-size M)
  (match M
    [`(λ (,x ,σ) ,M)
     (+ 1 (term-size M))]
    [`(,M ,N)
     (+ 1 (term-size M) (term-size N))]
    [_  ;; constant, number, or variable
     1]))

(module+ main
  
  (command-line #:args (num-exps)
                (call-with-output-file Output-Filename
                  (λ (out)
                    (void (write-string (gen-module (string->number num-exps))
                                        out)))
                  #:exists 'replace))

  (printf "\nOutput written to: ~a\n" Output-Filename))