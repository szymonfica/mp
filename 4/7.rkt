#lang racket

(require rackunit)

(define empty-queue
  (cons null null))

(define (empty? q)
    (null? (car q)))

(define (push-back x q)
  (if (empty? q)
      (cons (list x) null)
      (cons (car q) (cons x (cdr q)))))

(define (front q)
  (caar q))

(define (pop q)
  (if (null? (cdar q)) ;;; ???
      (pop (cons (reverse (cdr q)) null))
      (cons (cdar q) (cdr q))
      ))

;(pop (list (list 1 2 3) (list 6 5 4)))
(define tt (list (list 1 2 3) (list 6 5 4)))
(push-back 7 tt)

(pop tt)
