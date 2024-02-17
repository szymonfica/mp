#lang plait

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (leq))

(define-type Exp
  (numE [n : Number])
  (varE [x : Symbol])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (appE [f : Symbol] [es : (Listof Exp)]))

(define-type Fun
    (funD [f : Symbol] [xs : (Listof Symbol)] [e : Exp]))

(define-type Program
    (program [xs : (Listof Fun)] [e : Exp]))

;; parse ----------------------------------------

(define (parse-program [s : S-Exp]) : Program
    (if (s-exp-match? `{define {ANY ...} for ANY} s)
     (program (map parse-def (s-exp->list (list-ref (s-exp->list s) 1))) (parse (list-ref (s-exp->list s) 3))) 
     (error 'parse-program "invalid program")))

(define (parse-def [s : S-Exp]) : Fun
    (if (s-exp-match? `{fun SYMBOL {SYMBOL ...} = ANY} s)
     (funD
      (s-exp->symbol (list-ref (s-exp->list s) 1))
      (map s-exp->symbol (s-exp->list (list-ref (s-exp->list s) 2)))
      (parse (list-ref (s-exp->list s) 4)))
     (error 'parse-def "invalid function definition")))

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (fourth (s-exp->list s)))
          (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (fourth (s-exp->list s)))
           (parse (list-ref (s-exp->list s) 5)))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (opE (parse-op (s-exp->symbol (second (s-exp->list s))))
          (parse (first (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL {ANY ...}} s)
            (appE (s-exp->symbol (list-ref (s-exp->list s) 0))
                  (map parse (s-exp->list (list-ref (s-exp->list s) 1))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))

;; values

(define-type-alias Value Number)

(define-type Binding
  (bindV [name : Symbol] [val : Value])
  (bindF [name : Symbol] [xs : (Listof Symbol)] [e : Exp]))

(define (bind-sym-val [s : (Listof Symbol)] [v : (Listof Value)]) : (Listof Binding)
  (cond [(and (empty? s) (empty? v)) empty]
        [(empty? s) (error 'matching "symbol list is too short")]
        [(empty? v) (error 'matching "values list is too short")]
        [else (cons (bindV (first s) (first v)) (bind-sym-val (rest s) (rest v)))]))

;; environments

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env-var [env : Env] [n : Symbol] [v : Value]) : Env
  (cons (bindV n v) env))

(define (extend-env-fun [env : Env] [n : Symbol] [a : (Listof Symbol)] [b : Exp]) : Env
  (cons (bindF n a b) env))

(define (extend-env-list [env : Env] [bs : (Listof Binding)]) : Env
  (foldr (lambda (b e)
           (type-case Binding b
             [(bindV name val) (extend-env-var e name val)]
             [(bindF name args exp) (extend-env-fun e name args exp)]))
         env bs))

(define (lookup-env [n : Symbol] [env : Env]) : Binding
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound var/fun")]
    [(cons b rst-env)
     (type-case Binding b
       [(bindV name val)
        (if (eq? n name) b (lookup-env n rst-env))]
       [(bindF name xs e)
        (if (eq? n name) b (lookup-env n rst-env))])]))

(define (fun-env [xs : (Listof Fun)]) : Env
  (foldr (lambda (function e) (extend-env-fun e (funD-f function) (funD-xs function) (funD-e function))) mt-env xs))

;; primitive operations and the initial environment

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(leq) (lambda (a b) (if (<= a b) 0 7))]))

;; evaluation function

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) n]
    [(varE x)
     (type-case Binding (lookup-env x env)
       [(bindV n v) v]
       [else (error 'eval "not a variable")])]
    [(opE op l r)
     ((op->proc op) (eval l env) (eval r env))]
    [(ifE w e1 e2)
     (if (= 0 (eval w env))
         (eval e1 env)
         (eval e2 env))]
    [(letE x e1 e2)
     (eval e2 (extend-env-var env x (eval e1 env)))]
    [(appE f zs)
     (type-case Binding (lookup-env f env)
       [(bindF n xs e)
        (eval e (extend-env-list env (bind-sym-val xs (foldr (lambda (ee ys) (cons (eval ee env) ys)) empty zs))))]
       [else (error 'eval "not a function")])]))

(define (run [s : S-Exp]) : Value
  (let* ([exp (parse-program s)]
         [env (fun-env (program-xs exp))])
    (eval (program-e exp) env)))

