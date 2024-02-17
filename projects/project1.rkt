#lang racket

; Szymon Fica 337307

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join
         )


(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

;

(define (wanted? c xs)
  (cond
    [(empty? xs) #f]
    [(equal? c (car xs)) #t]
    [else (wanted? c (cdr xs))]
    )
  )

; Wstawianie

(define (table-insert row tab)
  
  (define (same-type? row sch)
    (define (single x column)
      (or
       (and (equal? 'boolean (column-info-type column))
            (boolean? x))
       (and (equal? 'number (column-info-type column))
            (number? x))
       (and (equal? 'string (column-info-type column))
            (string? x))
       (and (equal? 'symbol (column-info-type column))
            (symbol? x))))
  
  (define (f xs ys)
    (cond
      [(not (equal? (length xs) (length ys))) #f]
      [(and (null? xs) (null? ys)) #t]
      [(or (null? xs) (null? ys)) #f]
      [else
       (and (single (car xs) (car ys))
            (f (cdr xs) (cdr ys)))]))
  (f row sch))

  (if (same-type? row (table-schema tab))
      (table (table-schema tab) (cons row (table-rows tab)))
      (error 'failed)
      )
  )

; Projekcja

(define (table-project cols tab)
  
(define (f xs ys ans)
  (cond
    [(empty? xs) ans]
    [(wanted? (column-info-name (car xs)) cols) (f (cdr xs) (cdr ys ) (cons (car ys) ans))]
    [else (f (cdr xs) (cdr ys ) ans)]
    )        
  )

  (define (g li)
    (if (empty? li)
        empty
        (cons (reverse (f (table-schema tab) (car li) empty)) (g (cdr li) ))
    )
  )

  (define (h st wa ans)
    (cond
      [(empty? st) ans]
      [(wanted? (column-info-name (car st)) wa) (h (cdr st) wa (cons (car st) ans))]
      [else (h (cdr st) wa ans)]
      )
    )
  
  (table (reverse (h (table-schema tab) cols empty)) (g (table-rows tab)))
)
  

; Sortowanie

(define (table-sort cols tab)

(define (split xs)
  (if (or (null? xs) (null? (cdr xs))) (cons xs null)
      (let [(y (split (cdr xs)))] (cons (cons (car xs) (cdr y)) (car y)))))

(define (ll? sch xs ys tt)
  (cond
    [(equal? (column-info-name (car sch)) tt) (equal? (car xs) (car ys))]
    [else (ll? (cdr sch) (cdr xs) (cdr ys) tt)]
    )
  )

(define (lll? sch xs ys tt)
  (cond
    [(equal? (column-info-name (car sch)) tt)
     (cond
       [(equal? 'boolean (column-info-type (car sch))) (car xs)]
       [(equal? 'number (column-info-type (car sch))) (< (car xs) (car ys))]
       [(equal? 'string (column-info-type (car sch))) (string<? (car xs) (car ys))]
       [(equal? 'symbol (column-info-type (car sch))) (symbol<? (car xs) (car ys))]
       )
     ]
    [else (lll? (cdr sch) (cdr xs) (cdr ys) tt)]
    )
  )

(define (l? xs ys wa)
  (cond
    [(empty? wa) #t]
    [(not (ll? (table-schema tab) xs ys (car wa))) (lll? (table-schema tab) xs ys (car wa))]
    [else (l? xs ys (cdr wa))]
    )
  )

(define (merge xs ys)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(l?  (car xs) (car ys) cols) (cons (car xs) (merge (cdr xs) ys))]
    [else (cons (car ys) (merge xs (cdr ys)))]))

  (define (merge-sort xs)
    (if (or (null? xs) (null? (cdr xs))) xs
        (let [(y (split xs))] (merge (merge-sort (car y)) (merge-sort (cdr y))))))
  
  (table (table-schema tab) (merge-sort (table-rows tab)))
  )

; Selekcja

(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (table-select form tab)
  
  (define (get xs sch tt)
    (cond
      [(equal? (column-info-name (car sch)) tt) (car xs)]
      [else (get (cdr xs) (cdr sch) tt)]
    )
    )

  (define (eval fi xs)
    (cond
      [(and-f? fi) (and (eval (and-f-l fi) xs) (eval (and-f-r fi) xs))]
      [(or-f? fi) (or (eval (or-f-l fi) xs) (eval (or-f-r fi) xs))]
      [(not-f? fi) (not (eval (not-f-e fi) xs))]
      [(eq-f? fi) (equal? (get xs (table-schema tab) (eq-f-name fi)) (eq-f-val fi))]
      [(eq2-f? fi) (equal? (get xs (table-schema tab) (eq2-f-name)) (get xs (table-schema tab) (eq2-f-name2)))]
      [(lt-f? fi) (< (get xs (table-schema tab) (lt-f-name fi)) (lt-f-val fi))]
      )
    )

  (define (f xs ans)
    (cond
      [(empty? xs) ans]
      [(eval form (car xs)) (f (cdr xs) (cons (car xs) ans))]
      [else (f (cdr xs) ans)]
      )
    )

  (table (table-schema tab) (reverse (f (table-rows tab) empty)))
  )
  
 ;
; Zmiana nazwy


(define (table-rename col ncol tab)
  
  (define (f xs ans)
    (cond
      [(empty? xs) ans]
      [(equal? (column-info-name (car xs)) col)
       (f (cdr xs) (cons (column-info ncol (column-info-type (car xs))) ans))]
      [else (f (cdr xs) (cons (car xs) ans))]
      )
    )
  (table (reverse (f (table-schema tab) empty)) (table-rows tab))
  
  )

; Złączenie kartezjańskie

(define (table-cross-join tab1 tab2)
  (define (breeding xs ys)
    (cond
      [(empty? ys) empty]
      [else (cons (append xs (car ys)) (breeding xs (cdr ys)))]
      )
    )
  (define (f xs)
    (cond
      [(empty? xs) empty]
      [else (append (breeding (car xs) (table-rows tab2)) (f (cdr xs)))]
      )
    )
  
  (table (append (table-schema tab1) (table-schema tab2)) (f (table-rows tab1)))
  )

; Złączenie

(define (table-natural-join tab1 tab2)
  tab1
  
  )
