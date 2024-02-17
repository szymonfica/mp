#lang plait

; zadanie 1
(define (f1 a b) a)
(define (f2 a b c) (a c (b c)))
(define (f3 [a : (('a -> 'a) -> 'a)]) (a identity))

(define (f4 b c) (lambda (a) (pair (b a) (c a))))

(define (f6 [f : ('a -> (Optionof ('a * 'b)))] [y : 'a])
  (list (snd (some-v (f y))))
)

(define (apply f x) (f x)) ; (('a -> 'b) 'a -> 'b)
(define (compose f g) (lambda (x) (f (g x)))) ; (('a -> 'b) ('c -> 'a) -> ('c -> 'b))
(define (flip f) (lambda (x y) (f y x))) ; (('a 'b -> 'c) -> ('b 'a -> 'c))
(define (curry f) (lambda (x) (lambda (y) (f x y)))) ; (('a 'b -> 'c) -> ('a -> ('b -> 'c)))


;(define (f5 aoab a)
;  (if (some? (aoab a))
;                        (cons (snd (some-v (aoab a))) (f5 aoab (fst (some-v (aoab a))))) empty))



; zadanie 4

; scala liste list w jedna liste
(define (flatten xss)
  (if (empty? xss)
      empty
      (append (first xss) (flatten (rest xss)))))

; wykonuje operacje 'f' na kazdym elemencie z listy i zwraca jedna liste

(define (append-map f xs) (flatten (map f xs))) 

; zwraca liste list z elementem dolozonym na kazde kolejne miejsce

(define (insert y ys)
    (if (empty? ys)
        (list (list y))
        (cons (cons y ys) (map (lambda (zs) (cons (first ys) zs)) (insert y (rest ys))))))


(define (perms xs)
    (if (empty? xs)
      (list empty)
      (append-map (lambda (ys) (insert (first xs) ys)) (perms (rest xs)))))

; Zadanie 6


(define-type (Rose-Tree 'a)
  (leaf [val : 'a])
  (node [cs : (Listof (Rose-Tree 'a))]))

(define (fold-tree acc t)
  (cond [(leaf? t) (cons (leaf-val t) acc)]
        [else
         (foldr
          (λ (x xs) (fold-tree xs x)) acc (node-cs t))]))

(define rose-tree-expl
  (node
   (list
    (node (list (leaf 1) (leaf 2)))
    (node (list (leaf 3) (node (list (leaf 18) (leaf 5) (leaf 6))) (leaf 7)))
    (node (list (leaf 8))))))

; zadanie 7

(define-type Prop
   (var [v : String])
   (conj [l : Prop] [r : Prop])
   (disj [l : Prop] [r : Prop])
   (neg [f : Prop]) )

(define (free-vars2 prop)
  (cond ([var? prop] (list (var-v prop)))
        ([conj? prop] (append (free-vars2 (conj-l prop)) (free-vars2 (conj-r prop))))
        ([disj? prop] (append (free-vars2 (disj-l prop)) (free-vars2 (disj-r prop))))
        ([neg? prop] (free-vars2 (neg-f prop)))))

(define (free-vars prop)
  (hash-keys (hash (map (lambda (x) (pair x x)) (free-vars2 prop)))))

(define zm1 (var "x"))
(define zm2 (var "y"))

(define r1 (conj zm1 zm2))

; zadanie 8

(define (eval h prop)
  (type-case Prop prop
    [(var v) (some-v (hash-ref h v))]
    [(conj l r) (and (eval h l) (eval h r))]
    [(disj l r) (or (eval h l) (eval h r))]
    [(neg f) (not (eval h f))]))


; zadanie 3


 (curry compose)
; curry (('a 'b -> 'c) -> ('a -> ('b -> 'c)))
; compose - (('d -> 'e) ('f -> d') -> ('f -> 'e))
; 'a = ('d -> 'e)
; 'b = ('f -> d')
; 'c = ('f -> 'e)
; (curry compose) - ('a -> ('b -> 'c))) = (('_d -> '_e) -> (('_f -> '_d) -> ('_f -> '_e))))



 ((curry compose) (curry compose))
; 
; (curry compose) - (('_d -> '_e) -> (('_f -> '_d) -> ('_f -> '_e)))
; (curry compose) - (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))
; '_d - ('_a -> '_b)
; '_e - (('c -> '_a) -> ('_c -> '_b))
; ((curry compose) (curry compose)) - (('_f -> ('_a -> '_b)) -> ('_f -> (('c -> '_a) -> ('_c -> '_b))))


 ((curry compose) (curry apply))

; (curry compose) - (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))

; curry (('d 'e -> 'f) -> ('d -> ('e -> 'f)))
; apply - (('g -> 'h) 'g -> 'h)
; (curry apply) :
; 'd - ('g -> 'h)
; 'e - 'g
; 'f - 'h
; (curry apply) - (('_g -> '_h) -> ('_g -> '_h))

;'_a - ('_g -> '_h)
;'_b - ('_g -> '_h)
; ((curry compose) (curry apply)) - (('_c -> ('_g -> '_h)) -> ('_c -> ('_g -> '_h)))


((curry apply) (curry compose))
; (curry apply) - (('_g -> '_h) -> ('_g -> '_h))
; (curry compose) - (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))
; ((curry apply) (curry compose)) - (('_a -> '_b) -> (('_c -> '_a) -> ('_c -> '_b)))


(compose curry flip)
; compose - (('b -> 'c) ('a -> b') -> ('a -> 'c))
; curry - (('d 'e -> 'f) -> ('d -> ('e -> 'f)))
; flip - (('g 'h -> 'i) -> ('h 'g -> 'i))

; ('h 'g -> 'i) = ('d 'e -> 'f) => flip - (('e 'd -> 'f) -> ('d 'e -> 'f))

; 'c - ('d -> ('e -> 'f))
; 'a - ('e 'd -> 'f)
; (compose curry flip) - (('_e '_d -> '_f) -> ('_d -> ('_e -> '_f)))



