#lang plait


; zadanie 1

(define (f x) (+ x 1))

(define (g x) (* x 2))

(define xs (list 1 2 3 4))

(define h (lambda (x) (f (g x))))

; Dla dowolnych funkcji f i g oraz listy xs zachodzi
; (map f (map g xs)) = (map (lambda (x) (f (g x))) xs)

; Zasada indukcji dla list:
; Niech P będzie własnością list, taką, że:
; i)  P(empty)
; ii) Dla każdej wartości x i każdej listy xs,
;     jeśli P(xs) to P((cons x xs))
; Wówczas dla dowolnej listy xs zachodzi P(xs)

; Niech P(xs) := (map f (map g xs)) = (map (lambda (x) (f (g x))) xs)

; Trzeba pokazać:
; i) P(empty) 
;    (map f (map g empty)) = (map (lambda (x) (f (g x))) empty)
;    L = (map f (map g empty)) = (map f empty) = empty
;    P = (map (lambda (x) (f (g x))) empty) = empty


; ii) Dla dowolnych x i xs, t. że P(xs), zachodzi
;    (map f (map g (cons x xs))) = (map (lambda (x) (f (g x))) (cons x xs))
;
;    Weźmy dowolne x i xs i załóżmy P(xs)
;    pokażmy, że (map f (map g (cons x xs))) = (map (lambda (x) (f (g x))) (cons x xs))
;
;    L = (map f (map g (cons x xs))) = (map f (cons (map g x) (map g xs)) =
;        (cons (f  (g x)) (map f (map g xs)))
;
;    P = (map (lambda (x) (f (g x))) (cons x xs)) =
;        (cons ((lambda (x) (f (g x))) 2) (map (lambda (x) (f (g x))) xs)) =
;        (cons (f (g x)) (map (lambda (x) (f (g x))) xs))
;
; Na mocy zasady indukcji mamy P(xs) dla dowolnej listy xs

; zadanie 2

; Dla dowolnych list xs, ys
; ∃ zs - lista, (append xs ys) ≡ zs
;
; Dowód: przez indukcję strukturalną względem listy xs
; P(xs) := ∀ ys, ∃ zs - lista, (append xs ys) ≡ zs
;
; i) P(empty)
;    (append empty ys) = ys = zs
;
; ii) Załóżmy, że ∃ zs - lista, (append xs ys) = zs 
;     i pokażmy, że ∃ zs' - lista, (append (cons x xs) ys) = zs'
;
;     L = (append (cons x xs) ys) = (cons x (append xs ys)) (z zał. zs = (append xs ys))
;     L = (cons x zs) - wartość



; zadanie 3

(define-type (NNF 'v)
  (nnf-lit [polarity : Boolean] [var : 'v])
  (nnf-conj [l : (NNF 'v)] [r : (NNF 'v)])
  (nnf-disj [l : (NNF 'v)] [r : (NNF 'v)]))

(define v1 (nnf-lit #t 1))
(define v2 (nnf-lit #f 2))
(define v3 (nnf-lit #t 3))
(define v4 (nnf-lit #t 4))

(define fi (nnf-conj v1 v2))
(define fi2 (nnf-disj v3 v4))

; Zasada indukcji dla typu NNF
;
; Niech P będzie własnością formuł w NNF, taką, że:
;
; i)  P(x) - gdzie x to dowolny literał występujący w formule
; ii) Dla dowolnych formuł l r, jeśli P(l) i P(r) to P((nnf-conj l r)) i P((nnf-disj l r))
;
; Wówczas zachodzi P(fi) dla dowolnej formuły NNF
;

; zadanie 4

(define (neg-nff x)
  (cond
    [(nnf-lit? x) (nnf-lit (not (nnf-lit-polarity x)) (nnf-lit-var x))]
    [(nnf-conj? x) (nnf-disj (neg-nff (nnf-conj-l x)) (neg-nff (nnf-conj-r x)))]
    [(nnf-disj? x) (nnf-conj (neg-nff (nnf-disj-l x)) (neg-nff (nnf-disj-r x)))]))

;(neg-nff fi)


; P(x) = (neg-nnf (neg-nnf x)) ≡ x

; P((nnf-lit x)) = (neg-nnf (neg-nnf x)) = x
; L = (neg-nnf (neg-nnf φ)) = (neg-nnf (nnf-lit (not nnf-lit-polarity x)) (nnf-lit-var x))
; L = (nnf-lit (not (not nnf-lit-polarity x))) (nnf-lit-var x))
; L = (nnf-lit (nnf-lit-polarity x) (nnf-lit-var x)) = fi

; P((nnf-conj x)) = (neg-nnf (neg-nnf x)) ≡ x
; L = (neg-nnf (nnf-disj (neg-nnf (nnf-conj-l x)) (neg-nnf (nnf-conj-r x))))
; L = (nnf-conj (neg-nnf (neg-nnf (nnf-conj-l x)) (neg-nnf (neg-nnf (nnf-conj-r x))))
; L = (nnf-conj (nnf-conj-l x) (nnf-conj-r x)) = x = P


; zadanie 5

(define (wart x)
  (cond
    [(eq? x 1) #f]
    [(eq? x 2) #f]
    [(eq? x 3) #f]
    [(eq? x 4) #f]
    [(eq? x 5) #f]
    [(eq? x 6) #f]))

(define (eval-nnf w x)
  (cond
    [(nnf-lit? x) (if (nnf-lit-polarity x) (wart (nnf-lit-var x)) (not (wart (nnf-lit-var x)))) ]
    [(nnf-conj? x) (and (eval-nnf w (nnf-conj-l x)) (eval-nnf w (nnf-conj-r x)))]
    [(nnf-disj? x) (or (eval-nnf w (nnf-disj-l x)) (eval-nnf w (nnf-disj-r x)))]))


;(eval-nnf wart fi)
;(eval-nnf wart fi2)

; P(x) = (eval-nnf σ (neg-nnf x)) = (not (eval-nnf σ x)).

; P((nnf-lit x)) = (eval-nnf σ (neg-nnf x)) = (not (eval-nnf σ x))
; polarity #t
; L = (eval-nnf σ (nnf-lit (not (nnf-lit-polarity x)) (nnf-lit-var x)))
; L = (not (wart (nnf-lit-var x)))

; P = (not (eval-nnf σ x)) = (not (wart (nnf-lit-var x))) = L

; analogicznie dla polarity #f

; P((nnf-conj x)) = (eval-nnf σ (neg-nnf x)) = (not (eval-nnf σ x))

; L = (eval-nnf σ (neg-nnf x))
; L = (eval-nnf wart (nnf-disj (neg-nff (nnf-conj-l x)) (neg-nff (nnf-conj-r x))))


; zadanie 6

(define-type (Formula 'v)
  (var [var : 'v])
  (neg [f : (Formula 'v)])
  (conj [l : (Formula 'v)] [r : (Formula 'v)])
  (disj [l : (Formula 'v)] [r : (Formula 'v)]))

(define (to-nnf x)
  (cond
    [(var? x) (nnf-lit #t (var-var x))]
    [(neg? x) (cond
                
                [(var? (neg-f x)) (nnf-lit #f (var-var (neg-f x)))]
                [(neg? (neg-f x)) (to-nnf (neg-f (neg-f x)))]
                [(conj? (neg-f x)) (nnf-disj (to-nnf (neg (conj-l (neg-f x)))) (to-nnf (neg (conj-r (neg-f x)))))]
                [(disj? (neg-f x)) (nnf-conj (to-nnf (neg (disj-l (neg-f x)))) (to-nnf (neg (disj-r (neg-f x)))))]
                )]
    [(conj? x) (nnf-conj (to-nnf (conj-l x)) (to-nnf (conj-r x)))]
    [(disj? x) (nnf-disj (to-nnf (disj-l x)) (to-nnf (disj-r x)))]
    ))

(define v5 (var 5))
(define v6 (var 6))
(define fi3 (conj v5 v6))
(define fi4 (neg fi3))

; (to-nnf fi4)

; P(φ) = (eval-nnf σ (to-nnf φ)) ≡ (eval-formula σ φ)


; zdanie 7

(define (eval-formula wart x)
  (eval-nnf wart (to-nnf x))
  )

(eval-formula wart fi4)

; P(φ) = (eval-nnf σ (to-nnf φ)) ≡ (eval-formula σ φ)


(define (eval-f wart x)
  (cond
    [(var? x) (wart (var-var x))]
    [(neg? x) (not (eval-f wart (neg-f x)))]
    [(conj? x) (and (eval-f wart (conj-l x)) (eval-f wart (conj-r x)))]
    [(disj? x) (or (eval-f wart (disj-l x)) (eval-f wart (disj-r x)))]
    
  ))

(eval-f wart fi4)

; P(φ) = (eval-nnf σ (to-nnf φ)) ≡ (eval-formula σ φ)

; P((var x))

; L = (eval-nnf wart (nnf-lit #t (var-var x)))
; L = (wart (nnf-lit-var (to-nnf x)))
; P = (wart (var-var x)) = L

; P((conj x))

; L = (eval-nnf wart (nnf-conj (to-nnf (conj-l x)) (to-nnf (conj-r x))))
























