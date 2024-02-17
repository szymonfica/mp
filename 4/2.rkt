#lang racket


(define leaf 'leaf)                ; konstruktor liścia

(define (leaf? t)                  ; predykat
  (eq? t 'leaf))

(define (node v l r)               ; konstruktor węzła
  (list 'node v l r))

(define (node? t)                  ; predykat
  (and (list? t)
       (= (length t) 4)
       (eq? (first t) 'node)))

(define (node-val t)               ; selektory
  (second t))

(define (node-left t)
  (third t))

(define (node-right t)
  (fourth t))

(define (tree? t)                  ; predykat definiujący
  (or (leaf? t)                    ; nasz główny typ danych!
      (and (node? t)
           (tree? (node-left  t))
           (tree? (node-right t)))))

(define big-tree
  (node 1 (node 2 (node 4 leaf leaf)
                (node 5 leaf leaf))
        (node 3 (node 6 leaf leaf)
              (node 7 leaf leaf))))

; ============
; CWICZENIE 2
; =============

(define (fold-tree proc init t)
  (if (leaf? t)
      init
      (proc (node-val t)
            (fold-tree proc init (node-left t))
            (fold-tree proc init (node-right t)))))

(define (sum-tree t) (fold-tree + 0 t))

(define (flip t)
  (fold-tree (lambda (v l r) (node v r l)) leaf t))

(define (height t)
  (fold-tree (lambda (v l r) (+ 1 (max l r))) 0 t))

(define (tree-span t)
  (fold-tree (lambda (v l r) (cons (if l (car l) v) (if r (cdr r) v) ) ) false t))

(define (tree-max t)
  (fold-tree max (if (leaf? t) false (node-val t)) t))

(define (tree-flatten t)
    (fold-tree (lambda (x y z) (append y (list x) z)) null t)
)

(tree-span big-tree)
