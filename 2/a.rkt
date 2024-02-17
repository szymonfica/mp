
#lang plait

; zad 1
(define (f1 a b) a)
(define (f2 abc ab a) (abc a (ab a)))
(define (f3 aaa) (aaa (lambda (x) x)))
; (define (f4 ab ac) (lambda (a) (pair (ab a) (ac a))))
; (define (f5 aoab a) (if (some? (aoab a)) (cons (snd (some-v (aoab a))) (f5 aoab (fst (some-v (aoab a))))) empty)) 

; zad 4
(define (flatten xss) (if (empty? xss) empty (append (first xss) (flatten (rest xss)))))
(define (append-map f xs) (flatten (map f xs))) 

(define (insert y ys)
    (if (empty? ys)
        (list (list y))
        (cons (cons y ys) (map (lambda (zs) (cons (first ys) zs)) (insert y (rest ys))))))


(define (perms xs)
    (if (empty? xs)
      (list empty)
      (append-map (lambda (ys) (insert (first xs) ys)) (perms (rest xs)))))


; zad 7
( define-type Prop
   ( var [ v : String ])
   ( conj [ l : Prop ] [ r : Prop ])
   ( disj [ l : Prop ] [ r : Prop ])
   ( neg [ f : Prop ]) )

(define (free-vars2 prop)
  (cond ([var? prop] (list (var-v prop)))
        ([conj? prop] (append (free-vars2 (conj-l prop)) (free-vars2 (conj-r prop))))
        ([disj? prop] (append (free-vars2 (disj-l prop)) (free-vars2 (disj-r prop))))
        ([neg? prop] (free-vars2 (neg-f prop)))))

(define (free-vars prop)
  (hash-keys (hash (map (lambda (x) (pair x x)) (free-vars2 prop)))))

; zad 8
(define (eval h prop)
  (type-case Prop prop
    [(var v) (some-v (hash-ref h v))]
    [(conj l r) (and (eval h l) (eval h r))]
    [(disj l r) (or (eval h l) (eval h r))]
    [(neg f) (not (eval h f))]))