;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.2 Representing Queues
;;

; front
(define (front-ptr queue) (car queue))

; rear
(define (rear-ptr queue) (cdr queue))

; set! front
(define (set-front-ptr! queue item) (set-car! queue item))

; set! rear
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; null? front
(define (empty-queue? queue) (null? (front-ptr queue)))

; make queue
(define (make-queue) (cons '() '()))

; return front
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; insert queue to the rear
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond 
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))

; delete front
(define (delete-queue! queue)
  (cond 
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue))
    (else
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

; END