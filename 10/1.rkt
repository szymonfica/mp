#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp]))

;; parse ----------------------------------------

(define (parse-list proc  [xs : (Listof S-Exp)]) : Exp
  (if (= (length xs) 2)
      (opE proc (parse (first xs)) (parse (second xs)))
      (opE proc (parse (first xs)) (parse-list proc (rest xs)))))

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(equal? `+ (first (s-exp->list s)))
     (parse-list (add) (append (list `0 `0) (rest (s-exp->list s))))]

    [(equal? `* (first (s-exp->list s)))
     (parse-list (mul) (append (list `1 `1) (rest (s-exp->list s))))]

    [(equal? `- (first (s-exp->list s)))
     (if (= (length (s-exp->list s)) 2)
         (opE (sub) (numE 0) (parse (second (s-exp->list s))))
         (opE (sub)
              (parse (first (append (rest (s-exp->list s)) (list `0))))
              (parse-list (add) (rest (append (rest (s-exp->list s)) (list `0 `0 `0))))))]

    [(equal? `/ (first (s-exp->list s)))
     (if (= (length (s-exp->list s)) 2)
         (opE (div) (numE 1) (parse (second (s-exp->list s))))
         (opE (div)
              (parse (first (append (rest (s-exp->list s)) (list `1))))
              (parse-list (mul) (rest (append (rest (s-exp->list s)) (list `1 `1 `1))))))]

    [(equal? `/ (first (s-exp->list s)))
     (parse-list (div) (append (list `1 `1) (rest (s-exp->list s))))]
    
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [else (error 'parse "unknown operator")]))
                 
#|;(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE (add) (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (opE (mul) (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (opE (add)
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test/exn (parse `{+ 1})
            "invalid input")
  (test/exn (parse `{^ 1 2})
            "unknown operator"))
|#;
  
;; eval --------------------------------------

(define-type-alias Value Number)

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(div) /]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) n]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        2)
  (test (run `{+ 2 1})
        3)
  (test (run `{* 2 1})
        2)
  (test (run `{+ {* 2 3} {+ 5 8}})
        19))

;; printer ———————————————————————————————————-

(define (print-value [v : Value]) : Void
  (display v))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))