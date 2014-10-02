#lang racket

(require math/statistics
         redex/reduction-semantics
         "poly-lam.rkt"
         (prefix-in np: "non-poly.rkt"))

(define (size t)
  (match t
    [(cons a b)
     (+ 1 (size a) (size b))]
    [else 1]))

(define (calc-sizes gen/d trials max-d name)
  (for/list ([d (in-range 1 (add1 max-d))])
    (printf "~a; depth: ~a\n" name d)
    (define sizes (map size
                       (filter values
                               (for/list ([_ (in-range trials)])
                                 (gen/d d)))))
    (list d 
          (exact->inexact (mean sizes))
          (length sizes)
          (exact->inexact (variance sizes)))))

(module+ main
  (parameterize ([depth-dependent-order? #f])
    (call-with-output-file "pl-sizes-flat.rktd"
      (λ (out)
        (pretty-display 
         (calc-sizes (λ (d)
                       (generate-term poly-stlc
                                      #:satisfying
                                      (typeof • M ((list int) → (list int)))
                                      d))
                     1000 12 "pl")
         out))
      #:exists 'replace)
    (call-with-output-file "np-sizes-flat.rktd"
      (λ (out)
        (pretty-display 
         (calc-sizes (λ (d)
                       (generate-term np:poly-stlc
                                      #:satisfying
                                      (np:typeof • M ((list int) → (list int)))
                                      d))
                     1000 12 "np")
         out))
      #:exists 'replace)))
