;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.22.
;;

(define (make-queue)
  (let 
    ((front-ptr '())
     (rear-ptr '()))
    ;; internal procedures
    ; set! front
    (define (set-front-ptr! item) (set! front-ptr item))
    ; set! rear
    (define (set-rear-ptr! item)  (set! rear-ptr item))
    ; null? front
    (define (empty-queue?) (null? front-ptr))
    ; return front
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          front-ptr))
    ; insert queue to the rear
    (define (insert-queue! item)
      (let 
        ((new-pair (cons item '())))
        (cond 
          ((empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr!  new-pair)
           (front-queue))
          (else
            (set-cdr! rear-ptr new-pair)
            (set-rear-ptr! new-pair)
            (front-queue)))))
    ; delete front
    (define (delete-queue!)
      (cond 
        ((empty-queue?)
         (error "DELETE! called with an empty queue"))
        (else
          (set-front-ptr! (cdr front-ptr))
          (front-queue))))
    ; dispatch
    (define (dispatch m)
      (cond
        ((eq? m 'insert!) insert-queue!)
        ((eq? m 'delete!) (delete-queue!))
        (else
          (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

; test
(define (insert-queue! queue item) 
  ((queue 'insert!) item))
(define (delete-queue! queue) 
  (queue 'delete!))

(define q (make-queue))
(print (insert-queue! q 'a)) ; (a)
(print (insert-queue! q 'b)) ; (a b)
(print (insert-queue! q 'c)) ; (a b c)
(print (delete-queue! q)) ; (b c)

; END
