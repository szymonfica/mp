#lang plait

(define-type (Tree 'a)
  (leaf)
  (node2 [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)])
  (node3[l : (Tree 'a)] [elem_a : 'a] [m : (Tree 'a)] [elem_b : 'a] [r : (Tree 'a)])
  )

(define (minel t)
  (type-case (Tree 'a) t
    [(leaf) -10]
    [(node2 l x r) 
  )

(define (twothree? t)
  (type-case (Tree 'a) t
    [(leaf) #t]
    [(node2 l elem r) (and (< (node2-l t)
  )


(define (is-empty? q)
  (type-case (Queue 'a) q
    [(empty-queue) #t]
    [(queue h f r) #f]))