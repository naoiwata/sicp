;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.25.
;;

(define (make-table)
  (let
    ((local-table (list '*table*)))
    (define (lookup key)
      (let
        ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f))) 
    (define (insert! key value)
      (let
        ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table))))))
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc)  lookup)
        ((eq? m 'insert-proc!) insert!)
        (else
          (error "Unknown table"))))
    dispatch))

; test
(define q (make-table))
(define lookup (q 'lookup-proc))
(define insert! (q 'insert-proc!))

(insert! '(1) 'apple)
(insert! '(2) 'banana)
(insert! '(3) 'cat)
(insert! '(4) 'dog)

(print (lookup '(1))) ; apple
(print (lookup '(3))) ; cat

(insert! '(1 1) 'apple-01)
(insert! '(1 2) 'apple-02)
(insert! '(2 1) 'banana-01)
(insert! '(2 2) 'banana-02)

(print (lookup '(1 1))) ; apple-01
(print (lookup '(3))) ; cat
(print (lookup '(2 2))) ; banana-02

(insert! '(2 1 1) 'banana-01-01)
(insert! '(2 1 3) 'banana-01-03)

(print (lookup '(2 1 1))) ; banana-01-01
(print (lookup '(2 2 1))) ; #f

; compared with ex2.66
; http://github.com/naoiwata/sicp/blob/master/chapter2/q2.66.scm

; END