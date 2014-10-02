#lang racket

(require racket/cmdline
         racket/runtime-path
         redex/reduction-semantics
         "poly-lam.rkt"
         (prefix-in np: "non-poly.rkt")
         "unparse.rkt")

(provide (all-defined-out))

(define-runtime-path Mod-Template "ModSkeleton.hs")
(define term-depth (make-parameter 9))
(define Output-Filename "TestModule.hs")
(define Term-Sizes-Filename "termsizes.txt")

(define mod-temp-path (make-parameter Mod-Template))

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

(define non-poly? (make-parameter #f))

(define (attempt)
  (if (non-poly?)
      (generate-term np:poly-stlc
                     #:satisfying
                     ;; to change target type change
                     ;; ((list int) → (list int)) in the following
                     ;; expression to the appropriate type
                     (np:typeof • M ((list int) → (list int)))
                     (term-depth))
      (generate-term poly-stlc
                     #:satisfying
                     ;; to change target type change
                     ;; ((list int) → (list int)) in the following
                     ;; expression to the appropriate type
                     (typeof • M ((list int) → (list int)))
                     (term-depth))))

(define (one-term)
  (display ".")
  (flush-output)
  (match (attempt)
    [#f (one-term)]
    [`(typeof • ,M ,t) M]
    [`(np:typeof • ,M ,t) M]))

(define (mod-template-text)
  (call-with-input-file (mod-temp-path)
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
  
  (command-line 
   #:once-each
   [("-t" "--template") path "Module template path"
                        (mod-temp-path path)]
   [("-n" "--non-poly") "Use pre-instantiated constants"
                         (non-poly? #t)]
   [("-d" "--depth-target") depth "Target term depth (default 9)"
                            (term-depth (string->number depth))]
   #:args (num-exps)
                (call-with-output-file Output-Filename
                  (λ (out)
                    (void (write-string (gen-module (string->number num-exps))
                                        out)))
                  #:exists 'replace))

  (printf "\nOutput written to: ~a\n" Output-Filename))