#lang racket

(require rackunit)

(define (suma_kwadratow a b c )
    (cond
    [(and (<= a b) (<= a c)) (+ (* b b) (* c c))]
    [(and (<= b a) (<= b c)) (+ (* a a) (* c c))]
    [(and (<= c a) (<= c b)) (+ (* a a) (* b b))]))


(check-equal? (suma_kwadratow 2 3 4) 25)
(check-equal? (suma_kwadratow 9 7 7) 130)
(check-equal? (suma_kwadratow 9 9 9 ) 162)
