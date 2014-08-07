#lang racket

(require "poly-lam.rkt"
         racket/runtime-path)

(provide (all-defined-out))

(define ids (make-parameter #f))

(define charmap (make-parameter #f))

(define (unp-exp e)
  (parameterize ([charmap (make-hasheq)])
    (let recur ([e e])
      (match e
        [`(λ (,v ,v-type) ,e)
         (string-append "(\\" (recur v)
                        " -> (" (recur e) "))")]
        [`(,e @ ,t)
         (match e
           ['c:undefined
            (string-append "(" (recur e) ":: "
                           (unp-type t) ")")]
           [(or 'c:enumFromTo 'c:enumFromToX)
            (string-append "(" (recur e) 
                           ":: Int -> Int -> [Int])")]
           [_ (recur e)])]
        [`(,e1 ,e2)
         (string-append "(" (recur e1) " " (recur e2) ")")]
        [(? constant? c)
         (unp-constant c)]
        [(? number? n)
         (number->string n)]
        [(? symbol? x)
         (id-check (symbol->string x))]))))

(define (unp-type t)
  (match t
    ['int "Int"]
    ['bool "Bool"]
    [`(list ,t)
     (string-append "[" (unp-type t) "]")]
    [`(,dom → ,ran)
     (string-append "(" (unp-type dom) 
                    " -> " (unp-type ran) ")")]))

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
    ['c:+ "(+)"]
    ['c:- "(-)"]
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

(define (id-check s)
  (keyword-check
   (list->string
    (match (map fix-nonalpha (string->list s))
      [(cons (? char-upper-case? c1) rest)
       (cons (char-downcase c1)
             (cons c1 rest))]
      [sl sl]))))

(module+ test
  (check-equal? (id-check "Abc") "aAbc")
  (check-not-false (regexp-match #rx"[a-z]+"
                                 (id-check "a->"))))

(define (fix-nonalpha c)
  (cond 
    [char-alphabetic? c]
    [else
     (hash-ref (charmap) c (λ ()
                             (define gen-c (random-alpha))
                             (hash-set! (charmap) c gen-c)
                             gen-c))]))

(define (random-alpha)
  (list-ref lowercase-alphabet (random (length lowercase-alphabet))))

(define lowercase-alphabet
  (string->list "abcdefghijklmnopqrstuvwxyz"))


(define (keyword-check s)
  (if (or (set-member? keywords s)
          (set-member? constants s))
      (string-append "x" s)
      s))

(module+ test
  (check-equal? (keyword-check "->") "x->")
  (check-equal? (keyword-check "case") "xcase")
  (check-equal? (keyword-check "id") "xid"))

(define keyword-string 
  #<<KEYWORDEND>>
    !
    '
    ''
    -
    --
    -<
    -<<
    ->
    ::
    ;
    <-
    ,
    =
    =>
    >
    ?
    #
    *
    @
    [|
    |]
    \
    _
    `
    {
    }
    {-
    -}
    |
    ~
    as
    case
    of
    class
    data
    family
    instance
    default
    deriving
    instance
    do
    forall
    foreign
    hiding
    if
    then
    else
    import
    infix
    infixl
    infixr
    instance
    let
    in
    mdo
    module
    newtype
    proc
    qualified
    rec
    type
    family
    instance
    where
KEYWORDEND>>
  )

(define keywords (list->set (map string-trim (string-split keyword-string "\n"))))

(define (str-remove-parens str)
  (string-replace 
   (string-replace str "(" "")
   ")" ""))

(define-runtime-path poly-lam-path "poly-lam.rkt")

(define constants
  (list->set
   (map (compose str-remove-parens unp-constant)
    (call-with-input-file poly-lam-path
      (λ (in)
        (read-line in) ;; skip #lang
        (let loop ()
          (define next (read in))
          (match next
            [(? eof-object?)
             (error 'constants "didn't find constants")]
            [`(define-language poly-stlc ,stuff ...
                (c d ::= ,cs ...) ,other-stuff ...)
            cs]
            [_
             (loop)])))))))



