;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.23.
;;

; queue
(define (make-queue)
  (cons '() '()))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "no queue")
      (value-node (front-ptr queue))))
(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "no queue")
      (value-node (rear-ptr queue))))
; ptr
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue node)
  (set-car! queue node))
(define (set-rear-ptr! queue node)
  (set-cdr! queue node))
; node
(define (make-node value)
  (list value '() '()))
(define (value-node node)
  (car node))
(define (next-node node)
  (cddr node))
(define (prev-node node)
  (cadr node))
(define (set-next-node! node next)
  (set-cdr! (cdr node) next))
(define (set-prev-node! node prev)
  (set-car! (cdr node) prev))
; insert
(define (front-insert-queue! queue value)
  (let
    ((new-node (make-node value)))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-node)
       (set-rear-ptr!  queue new-node)
       queue)
      (else
        (set-next-node! new-node (front-ptr queue))
        (set-rear-ptr!  queue new-node)
        queue))))
(define (rear-insert-queue! queue value)
  (let
    ((new-node (make-node value)))
    (cond
      ((empty-queue? queue)
       (set-front-ptr! queue new-node)
       (set-rear-ptr!  queue new-node)
       queue)
      (else
        (set-prev-node! new-node (rear-ptr queue))
        (set-next-node! (rear-ptr queue) new-node)
        (set-rear-ptr!  queue new-node)
        queue))))
; delete
(define (front-delete-queue! queue)
  (cond
    ((empty-queue? queue)
     (error "no queue"))
    (else
      (set-front-ptr! queue (next-node (front-ptr queue))))))
(define (rear-delete-queue! queue)
  (cond
    ((empty-queue? queue)
     (error "no queue"))
    (else
      (set-rear-ptr! queue (prev-node (rear-ptr queue))))))
; display
(define (display-queue queue)
  (define (display-queue-internal q)
    (cond ((eq? q (rear-ptr queue))
           (display " ")
           (display (value-node q)))
          (else
           (begin (display " ")
                  (display (value-node q))
                  (display-queue-internal (next-node q))))))
  (if (empty-queue? queue)
      (display "empty queue\n")
      (begin
        (display "(")
        (display-queue-internal (front-ptr queue))
        (display ")\n"))))

; test
(define q (make-queue))
(display-queue q) ; empty queue
(front-insert-queue! q 'a)
(display-queue q) ; (a)
(rear-insert-queue! q 'b)
(display-queue q) ; (a b)
(front-delete-queue! q)
(display-queue q) ; (b)
(rear-insert-queue! q 'c)
(front-delete-queue! q)
(display-queue q) ; (c)

; END