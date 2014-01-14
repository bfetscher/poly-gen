#lang racket/base

(require redex/reduction-semantics
         racket/match)

(provide (all-defined-out))

(define-language poly-stlc
  (M N ::= 
     (λ (x σ) M)
     (M N)
     C
     number
     x)
  (C ::=
     [C @ σ]
     c)
  (σ τ ::= 
     int
     bool
     (σ → τ)
     (list σ))
  (γ ::=
     int
     (γ → γ)
     (list γ)
     α)
  (Γ ::=
     (x σ Γ)
     •)
  (Σ Y ::= 
     (∀ α Σ)
     γ)
  (x y ::= variable-not-otherwise-mentioned)
  (α β ::= variable-not-otherwise-mentioned)
  (c d ::= c:seq
           c:id
           c:+
           c:+1
           c:-
           c::
           c:enumFromTo
           ;c:enumFromTo'
           c:nil
           c:head
           c:tail
           c:take
           c:!!
           c:length
           c:filter
           c:map
           c:null
           c:++
           c:odd
           c:even
           c:&&
           c:||
           c:not
           c:True
           c:False
           c:foldr
           c:==1
           c:==2
           c:==3
           c:case1
           c:undefined))

(define-judgment-form poly-stlc
  #:mode (typeof I I O)
  
  [---------------------
   (typeof Γ number int)]
  
  [---------------------
   (typeof Γ c:True bool)]
  
  [---------------------
   (typeof Γ c:False bool)]
  
  [(typeof-C C τ)
   --------------
   (typeof Γ C τ)]
  
  [(where τ (lookup Γ x))
   ----------------------
   (typeof Γ x τ)]
  
  [(typeof (x σ Γ) M σ_2)
   --------------------------------
   (typeof Γ (λ (x σ) M) (σ → σ_2))]
  
  [(typeof Γ M (σ → σ_2))
   (typeof Γ M_2 σ)
   ----------------------
   (typeof Γ (M M_2) σ_2)])

(define-judgment-form poly-stlc 
  #:mode (typeof-C I O)
  
  [(const-type c (∀ α γ))
   (where σ (t-subst γ α τ))
   ------------------------------
   (typeof-C [c @ τ] σ)]
  
  [(const-type c (∀ α (∀ β γ)))
   (where γ_1 (t-subst γ β τ_2))
   (where σ (t-subst γ_1 α τ_1))
   ------------------------------
   (typeof-C [[c @ τ_1] @ τ_2] σ)]
  
  #;[(const-type c (∀ α (∀ β (∀ α_2 γ))))
   (where γ_0 (t-subst γ α_2 τ_3))
   (where γ_1 (t-subst γ_0 β τ_2))
   (where σ (t-subst γ_1 α τ_1))
   ------------------------------
   (typeof-C [[[c @ τ_1] @ τ_2] @ τ_3] σ)])

(define-metafunction poly-stlc
  lookup : Γ x -> σ or #f
  [(lookup (x σ Γ) x)
   σ]
  [(lookup (x σ Γ) x_2)
   (lookup Γ x_2)]
  [(lookup • x)
   #f])

(define-judgment-form poly-stlc
  #:mode (const-type I O)
  [(const-type 
    c:seq (∀ a (∀ b (a → (b → b)))))]
  [(const-type 
    c:id (∀ a (a → a)))]
  [(const-type 
    c:+ (∀ a (int → (int → int))))]
  [(const-type 
    c:+1 (∀ a (int → int)))]
  [(const-type 
    c:- (∀ a (int → (int → int))))]
  [(const-type
    c:: (∀ a (a → ((list a) → (list a)))))]
  [(const-type
    c:enumFromTo (∀ a (int → (int → (list int)))))]
  [(const-type
    c:enumFromToX (∀ a (int → (int → (list int)))))]
  [(const-type
    c:nil (∀ a (list a)))]
  [(const-type
    c:head (∀ a ((list a) → a)))]
  [(const-type
    c:tail (∀ a ((list a) → (list a))))]
  [(const-type
    c:take (∀ a (int → ((list a) → (list a)))))]
  [(const-type
    c:!! (∀ a ((list a) → (int → a))))]
  [(const-type
    c:length (∀ a ((list a) → int)))]
  [(const-type
    c:filter (∀ a ((a → bool) → ((list a) → (list a)))))]
  [(const-type
    c:map (∀ a (∀ b ((a → b) → ((list a) → (list b))))))]
  [(const-type
    c:null (∀ a ((list a) → bool)))]
  [(const-type
    c:++ (∀ a ((list a) → ((list a) → (list a)))))]
  [(const-type
    c:odd (∀ a (int → bool)))]
  [(const-type
    c:even (∀ a (int → bool)))]
  [(const-type
    c:&& (∀ a (bool → (bool → bool))))]
  [(const-type
    c:|| (∀ a (bool → (bool → bool))))]
  [(const-type
    c:not (∀ a (bool → bool)))]
  [(const-type
    c:foldr (∀ a (∀ b ((a → (b → b)) → (b → ((list a) → b))))))]
  [(const-type
    c:==1 (∀ a (int → (int → bool))))]
  [(const-type
    c:==2 (∀ a (bool → (bool → bool))))]
  [(const-type
    c:==3 (∀ a ((list int) → ((list int) → bool))))]
  [(const-type
    c:case1 (∀ a (∀ b ((a → ((list a) → b)) → (b → ((list a) → b))))))]
  [(const-type
    c:undefined (∀ a a))])

(define-metafunction poly-stlc
  t-subst : γ α τ -> γ
  [(t-subst int α τ)
   int]
  [(t-subst α α τ)
   τ]
  [(t-subst α β τ)
   α]
  [(t-subst (list γ) α τ)
   (list (t-subst γ α τ))]
  [(t-subst (γ → γ_2) α τ)
   ((t-subst γ α τ) → (t-subst γ_2 α τ))])


(define (generate-n-terms n)
  (filter 
   values
   (for/list ([_ (in-range n)])
     (match (generate-term poly-stlc
                           #:satisfying
                           (typeof • M ((list int) → (list int)))
                           5)
       [#f #f]
       [`(typeof • ,M ((list int) → (list int))) M]))))
