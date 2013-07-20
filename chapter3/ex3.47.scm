;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.47.
;;

; ------------------------------------------------------------------------
; question (a)
; ------------------------------------------------------------------------

(define (make-semaphore n)
  ((let 
     ((mutex (make-mutex))
      (process-num 0))
     (define (the-semaphore m)
       (cond
         ((eq? m 'acquire)
          (mutex m)
          (if (< n process-num)
              (begin 
                (mutex 'release)
                (the-semaphore m))
              (begin
                (set! process-num (+ process-num 1))
                (mutex 'release))))
         ((eq? m 'release)
          (mutex 'acquire)
          (if (< 0 process-num)
              (set! process-num (- process-num 1))
              (mutex 'release)))
         (else
           (error "unknown request" m))))
     the-semaphore))

; ------------------------------------------------------------------------
; question (b)
; ------------------------------------------------------------------------

(define (make-semaphore n)
  (let 
    ((process-num 0)
     (cell (list #f)))
    (define (the-semaphore m)
      (cond
        ((eq? m 'acquire)
         (if (or (test-and-set! cell) (<= n process-num))
             (the-semaphore m)
             (begin 
               (set! process-num (+ process-num 1))
               (clear! cell))))
        ((eq? m 'release)
         (if (< 0 process-num)
              (set! process-num (- process-num 1))
              (clear! cell)))
        (else
          (error "unknown request" m))))
    the-semaphore))

; END