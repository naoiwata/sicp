;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.27.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

; former solution
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
      (+ (fib (- n 1))
         (fib (- n 2))))))

; solution by using memo
(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert! key value table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

(define (memorize f)
  (let
    ((table (make-table)))
    (lambda (x)
      (let
        ((priviously-coumputed-result (lookup x table)))
        (or priviously-coumputed-result
            (let
              ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memorize 
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (fib (- n 1))
             (fib (- n 2))))))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (memo-fib 10)) ; 55
(print (memo-fib 40)) ; 102334155

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; memo-fib -> (memorize fib)

; END