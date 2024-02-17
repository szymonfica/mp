#lang racket

; zadanie 3

(define (suffixes xs)

  (if (empty? xs)
      empty
      (cons xs (suffixes (cdr xs)))
      )
  )

(define/contract (suffixes2 xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (empty? xs)
      empty
      (cons xs (suffixes2 (cdr xs)))
      )
  )

(define dumb (time (suffixes (range 10))))
(define stupid (time (suffixes2 (range 10))))

; zadanie 4

(define/contract (f1 a b)
  (parametric->/c [a b] (-> a b a))
  a
  )

(define/contract (f2 f g x)
  (parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
  (f x (g x))
  )

(define/contract (f3 f g x)
  (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
  (lambda (x) (f (g x)))
  )

(define/contract (f4 f)
  (parametric->/c [a] (-> (-> (-> a a) a) a))
  (f f)
  )

; zadanie 5

(define/contract (foldl-map f a xs)
  (parametric->/c [a acc c] (-> (-> a acc (cons/c c acc)) acc (listof a) (cons/c (listof c) acc)))
   (define (it a xs ys)
      (if (null? xs)
          (cons (reverse ys) a)
          (let [(p (f (car xs) a))]
            (it (cdr p)
                 (cdr xs)
                 (cons (car p) ys)))))
   (it a xs null))

;(foldl-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3))

; zadanie 6

(define/contract (my-foldr f x xs)
  (parametric->/c [a b] (-> (-> a b b) b (listof a) b))
  ;(parametric->/c [a] (-> (-> a a a) a (listof a) a))
  (if (null? xs)
      x
      (f (car xs) (my-foldr f x (cdr xs)))))


(define/contract (my-foldr2 f x xs)
  (parametric->/c [a] (-> (-> a a a) a (listof a) a))
  ;(parametric->/c [a b] (-> (-> a b b) b (listof a) b))
  (if (null? xs)
      x
      (f x (my-foldr2 f x (cdr xs)))))


(my-foldr + 0 '(1 2 3 4 5))
(my-foldr2 + 0 '(1 2 3 4 5))





















