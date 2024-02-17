#lang racket

; zadanie 3

(provide
 ;mqueue-front
 mqueue?

 nonempty-mqueue?
 
 (contract-out
   [mqueue-empty?      (-> mqueue? boolean?)]
   [make-mqueue        (-> mqueue?)]
   [mqueue-push-back   (-> mqueue? any/c void?)]
   [mqueue-push-front  (-> mqueue? any/c void?)]
   [mqueue-pop-back    (-> mqueue? any/c)]
   [mqueue-pop-front   (-> mqueue? any/c)]))


(struct mqueue
  ([front #:mutable]
   [back  #:mutable]))

(define (make-mqueue)
  (mqueue null null))

(define (mqueue-empty? q)
  (and (null? (mqueue-front q))
       (null? (mqueue-back  q))))

(define (nonempty-mqueue? q)
  (and (mqueue? q) (mpair? (mqueue-front q)) (mpair? (mcdr (mqueue-front q)))))


(define (mqueue-push-back q x)
  (define p (mcons x (mcons null null)))
  (cond
    [(mqueue-empty? q) (set-mqueue-front! q p)]
    [else
     (set-mcdr! (mcdr (mqueue-back q)) p)
     (set-mcar! (mcdr p) (mqueue-back q))
     ])
  (set-mqueue-back! q p))

(define (mqueue-push-front q x)
  (define p (mcons x (mcons null null)))
  (cond
    [(mqueue-empty? q) (set-mqueue-back! q p)]
    [else
     (set-mcar! (mcdr (mqueue-front q)) p)
     (set-mcdr! (mcdr p) (mqueue-front q))
     ])
  (set-mqueue-front! q p))

(define/contract (mqueue-pop-back q)
  (-> nonempty-mqueue? any/c)
  (define ans (mcar (mqueue-back q)))
  (cond
    [(eq? (mqueue-front q) (mqueue-back q))
     (set-mqueue-front! q null) (set-mqueue-back! q null)]
    [else
     (define p (mcar (mcdr (mqueue-back q)))) ; przedostatni element
     (set-mqueue-back! q p)
     (set-mcdr! (mcdr p) null)])
  ans
  
  )

(define/contract (mqueue-pop-front q)
  (-> nonempty-mqueue? any/c)
  (define ans (mcar (mqueue-front q)))
  (cond
    [(eq? (mqueue-front q) (mqueue-back q))
     (set-mqueue-front! q null) (set-mqueue-back! q null)]
    [else
     (define p (mcdr (mcdr (mqueue-front q)))) ; drugi element
     (set-mqueue-front! q p)
     (set-mcar! (mcdr p) null)
     ])
    ans
    )



