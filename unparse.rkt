#lang racket

(require "poly-lam.rkt")

(provide (all-defined-out))

(define ids (make-parameter #f))

(define (unp-exp e)
  (match e
    [`(λ (,v ,v-type) ,e)
     (string-append "(\\" (unp-exp v)
                    " -> (" (unp-exp e) "))")]
    [`(,e @ ,t)
     (if (annotate-exp? e)
         (string-append "(" (unp-exp e) ":: "
                        (unp-type t) ")")
         (unp-exp e))]
    [`(,e1 ,e2)
     (string-append "(" (unp-exp e1) " " (unp-exp e2) ")")]
    [(? constant? c)
     (unp-constant c)]
    [(? number? n)
     (number->string n)]
    [(? symbol? x)
     (fix-var-case (symbol->string x))]))
  
(define (unp-type t)
  (match t
    ['int "Int"]
    ['bool "Bool"]
    [`(list ,t)
     (string-append "[" (unp-type t) "]")]
    [`(,dom → ,ran)
     (string-append "(" (unp-type dom) 
                    " -> " (unp-type ran) ")")]))

(define (fix-var-case var)
  (match (string->list var)
    [(cons c1 rest)
     (if (char-upper-case? c1)
          (list->string
           (cons (char-downcase c1)
                 (cons c1 rest)))
          var)]))

  

(define (annotate-exp? e)
  (match e
    ['c:undefined #t]
    [_ #f]))

(define (constant? c)
  (and (symbol? c)
       (regexp-match #rx"c:.*"
                     (symbol->string c))))

(define (unp-constant c)
  (match c
    ['c:!! "(!!)"]
    ['c:nil "[]"]
    ['c:: "(:)"]
    ['c:+1 "(+1)"]
    ['c:enumFromToX "enumFromTo'"]
    ['c:++ "(++)"]
    ['c:&& "(&&)"]
    ['c:or "(||)"]
    ['c:==1 "(==)"]
    ['c:==2 "(==)"]
    ['c:==3 "(==)"]
    [_ (second
        (regexp-match #rx"c:(.*)"
                      (symbol->string c)))]))

(module+ test
  (require rackunit)
  (check-equal? (unp-exp 'c:tail) "tail")
  (check-equal? (unp-exp 'c:!!) "(!!)")
  (check-equal? (unp-exp '(λ (x int) x))
                "(\\x -> (x))")
  (check-equal? (unp-exp '((λ (x int) x) ((c:: 1) (nil @ (list int)))))
                "((\\x -> (x)) (((:) 1) nil))")
  (check-equal? (unp-exp '(c:map @ int)) "map")
  (check-equal? (unp-exp '(c:undefined @ (list int))) 
                "(undefined:: [Int])"))
                         
