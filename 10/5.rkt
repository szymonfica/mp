#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div)
  (eql)
  (leq))

(define-type Proc
  (car)
  (cdr)
  (null?))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op]
       [l : Exp]
       [r : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
  (condE [cs : (Listof (Exp * Exp))])
  (nullE)
  (consE [car : Exp]
         [cdr : Exp])
  (listE [es : (Listof Exp)])
  (procE [pr : Proc]
         [es : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{cond ANY ...} s)
     (condE (parse-cond (rest (s-exp->list s))))]
    [(s-exp-match? `null s)
     (nullE)]
    [(s-exp-match? `{cons ANY ANY} s)
     (consE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{list ANY ...} s)
     (listE (parse-list (rest (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (procE (parse-proc (s-exp->symbol (first (s-exp->list s))))
            (parse (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-cond [ss : (Listof S-Exp)]) : (Listof (Exp * Exp))
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (parse (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-cond ss))
         (error 'parse "invalid input: cond"))]))

(define (parse-list [ss : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) ss
    [empty
     empty]
    [(cons s ss)
     (if (s-exp-match? `ANY s)
         (cons (parse s) (parse-list ss))
         (error 'parse "invalid input: list"))]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

(define (parse-proc [proc : Symbol]) : Proc
  (cond
    [(eq? proc 'car) (car)]
    [(eq? proc 'cdr) (cdr)]
    [(eq? proc 'null?) (null?)]
    [else (error 'parse "unknown procedure")]))
                
(module+ test
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
  (test (parse `{if {= 0 1} {* 3 4} 8})
        (ifE (opE (eql) (numE 0) (numE 1))
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
  (test (parse `{cons 1 {cons 2 null}})
        (consE (numE 1) (consE (numE 2) (nullE))))
  (test (parse `{car {cdr {list 1 2 3 4 5 6}}})
        (procE (car)
               (procE (cdr) (listE
                             (list(numE 1) (numE 2) (numE 3) (numE 4) (numE 5) (numE 6))))))
   (test/exn (parse `{{+ 1 2}})
            "invalid input")
;  (test/exn (parse `{+ 1})
;            "invalid input")
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  (test (parse `{cond {{= 0 1} {* 3 4}}
                      {{= 1 1} 8}})
        (condE (list (pair (opE (eql) (numE 0) (numE 1))
                           (opE (mul) (numE 3) (numE 4)))
                     (pair (opE (eql) (numE 1) (numE 1))
                           (numE 8))))))
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (nullV)
  (consV [car : Value]
         [cdr : Value]))

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

(define (proc->proc [pr : Proc]) : (Value -> Value)
  (type-case Proc pr
    [(car)
     (λ (v)
       (type-case Value v
         [(consV car cdr) car]
         [else (error 'eval "type error")]))]
    [(cdr)
     (λ (v)
       (type-case Value v
         [(consV car cdr) cdr]
         [else (error 'eval "type error")]))]
    [(null?)
     (λ (v)
       (type-case Value v
         [(nullV) (boolV #t)]
         [(consV car cdr) (boolV #f)]
         [else (error 'eval "type error")]))]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(condE cs)
     (eval (cond->if cs))]
    [(nullE) (nullV)]
    [(consE car cdr)
     (type-case Value (eval cdr)
       [(nullV) (consV (eval car) (nullV))]
       [(consV car-aux cdr-aux) (consV (eval car) (consV car-aux cdr-aux))]
       [else
        (error 'eval "type error")])]
    [(listE es)
     (eval (list->cons es))]
    [(procE pr es)
     ((proc->proc pr) (eval es))]))

(define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
  (type-case (Listof (Exp * Exp)) cs
    [empty
     (numE 42)]
    [(cons c cs)
     (ifE (fst c)
          (snd c )
          (cond->if cs))]))

(define (list->cons [es : (Listof Exp)]) : Exp
  (type-case (Listof Exp) es
    [empty (nullE)]
    [(cons e es)
     (consE e (list->cons es))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        (numV 2))
  (test (run `{+ 2 1})
        (numV 3))
  (test (run `{* 2 1})
        (numV 2))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (numV 19))
  (test (run `{= 0 1})
        (boolV #f))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (numV 8))
  (test (run `{cons 2 {cons 3 null}})
        (consV (numV 2) (consV (numV 3) (nullV))))
  (test/exn (run `{cons 2 {cons null 2}})
            "type error")
  (test (run `{cond {{= 0 1} {* 3 4}}
                    {{= 1 1} 8}})
        (numV 8)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(nullV) "null" ]
    [(consV car cdr)
     (string-append "(cons "
                    (string-append (value->string car)
                                   (string-append " "
                                                   (string-append (value->string cdr) ")"))))]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))