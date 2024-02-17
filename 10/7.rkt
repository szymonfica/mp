#lang plait

(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and a1) a1]
    [(my-and a1 a2 ...)
     (if a1 (my-and a2 ...) a1)]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #f]
    [(my-or a1) a1]
    [(my-or a1 a2 ...) (if a1 a1 (my-or a2 ...))]))

(define-syntax my-let
  (syntax-rules ()
    [(my-let () a1) a1]
    [(my-let ([a1 b1] ...) f)
     ((lambda (a1 ...) f) b1 ...)]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () a1) a1]
    [(my-let* ([a1 b1] [a2 b2] ...) f)
     ((lambda (a1) (my-let* ([a2 b2] ...) f)) b1)]))
