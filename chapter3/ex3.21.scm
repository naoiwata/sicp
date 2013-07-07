  ;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.21.
;;

; ----- 3.3.2 -----------------------------------------
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
  (let 
    ((new-pair (cons item '())))
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

; ----- BEN ------------------------------------------
(define q1 (make-queue))
(insert-queue! q1 'a) ; ((a) a)
(insert-queue! q1 'b) ; ((a b) b)
(delete-queue! q1) ; ((b) b)
(delete-queue! q1) ; (() b)

; ----- solution ------------------------------------
(define print-queue
  (lambda (queue)
    (newline)
    (display (front-ptr queue))))

; test
(define q2 (make-queue))
(print-queue q2) ; ()
(insert-queue! q2 'a)
(print-queue q2) ; (a)
(insert-queue! q2 'b)
(print-queue q2) ; (a b)
(delete-queue! q2)
(print-queue q2) ; (b)
(delete-queue! q2)
(print-queue q2) ; ()

; 空リストにinsert-queue!を処理させた場合、このキューのfrontとrearは同じポインタをさすので、schemeの解釈系がキューのfrontを表現する際にrearも含めて印字する為

; END