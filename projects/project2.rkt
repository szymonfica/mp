#lang racket
(require data/heap)
(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))


; sim

(struct event (time action) #:transparent)
(struct sim (time actions) #:mutable #:transparent)

(define (make-sim)
  (sim 0 (make-heap (lambda (a b) (<= (event-time a) (event-time b))))))

(define (sim-wait! S time-added)
  (let ([time-end (+ (sim-time S) time-added)])
    (define (rec-sim-wait!)
      (if (and
           (< 0 (heap-count (sim-actions S)))
           (>= time-end (event-time (heap-min (sim-actions S)))))
          (let ((first-action (heap-min (sim-actions S))))
            (begin
              (set-sim-time! S (event-time first-action))
              (heap-remove-min! (sim-actions S))
              ((event-action first-action))
              (rec-sim-wait!)))
          (void)))
    (rec-sim-wait!)
    (set-sim-time! S time-end)))

(define (sim-add-action! S t a)
  (heap-add! (sim-actions S) (event (+ t (sim-time S)) a)))

; wire

(struct wire (value xs sim) #:mutable #:transparent)

(define (make-wire SIM)
  (wire #f empty SIM))

(define (wire-on-change! W act)
  (act)
  (set-wire-xs! W (cons act (wire-xs W))))

#;(define (run-list xs)
  (if (empty? xs)
      (void)
      (begin
        ((car xs))
        (run-list (cdr xs)))))

(define (wire-set! W x)
    (let ([y (wire-value W)])
        (if (equal? x y)
            (void)
            (begin
              (set-wire-value! W x)
              (for-each (lambda (x) (x)) (wire-xs W))))))

; bus

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

; gate

(define not-gate-dalay 1)
(define and-gate-dalay 1)
(define nand-gate-dalay 1)
(define or-gate-dalay 1)
(define nor-gate-dalay 1)
(define xor-gate-dalay 2)

(define (gate-not out in)
  (define (help)
    (let ([new-v (not (wire-value in))])
          (sim-add-action!
           (wire-sim out) not-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in help))

(define (gate-and out in1 in2)
  (define (help)
    (let ([new-v (and (wire-value in1) (wire-value in2))])
          (sim-add-action!
           (wire-sim out) and-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in1 help)
    (wire-on-change! in2 help))

(define (gate-nand out in1 in2)
  (define (help)
    (let ([new-v (nand (wire-value in1) (wire-value in2))])
          (sim-add-action!
           (wire-sim out) nand-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in1 help)
    (wire-on-change! in2 help))

(define (gate-or out in1 in2)
  (define (help)
    (let ([new-v (or (wire-value in1) (wire-value in2))])
          (sim-add-action!
           (wire-sim out) or-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in1 help)
    (wire-on-change! in2 help))

(define (gate-nor out in1 in2)
  (define (help)
    (let ([new-v (nor (wire-value in1) (wire-value in2))])
          (sim-add-action!
           (wire-sim out) nor-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in1 help)
    (wire-on-change! in2 help))

(define (gate-xor out in1 in2)
  (define (help)
    (let ([new-v (xor (wire-value in1) (wire-value in2))])
          (sim-add-action!
           (wire-sim out) xor-gate-dalay (lambda () (wire-set! out new-v)))))
    (wire-on-change! in1 help)
    (wire-on-change! in2 help))


(define (wire-not in) 
  (let ([out (make-wire (wire-sim in))])
    (gate-not out in) out))

(define (wire-and in1 in2)
  (let ([out (make-wire (wire-sim in1))])
    (gate-and out in1 in2) out))

(define (wire-nand in1 in2)
  (let ([out (make-wire (wire-sim in1))])
    (gate-nand out in1 in2) out))

(define (wire-or in1 in2)
  (let ([out (make-wire (wire-sim in1))])
    (gate-or out in1 in2) out))

(define (wire-nor in1 in2)
  (let ([out (make-wire (wire-sim in1))])
    (gate-nor out in1 in2) out))

(define (wire-xor in1 in2)
  (let ([out (make-wire (wire-sim in1))])
    (gate-xor out in1 in2) out))

; flip-flop

(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))


