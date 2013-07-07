;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.25.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-table)
  (let
    ((local-table (list '*table*)))
    (define (lookup key)
      (define (iter key-iter table)
        (if (null? key-iter)
            #f
            (let
              ((sub-table (assoc (car key-iter) (cdr table))))
              (if sub-table
                  (if (null? (cdr key-iter))
                      (cdr sub-table)
                      (iter (cdr key-iter) sub-table))
                  #f))))
      (iter key local-table))
    (define (insert! key val)
      (define (iter key-iter table)
        (if (null? key-iter)
            #f
            (let
              ((sub-table (assoc (car key-iter) (cdr table))))
              (if sub-table
                  (if (null? (cdr key-iter))
                      (set-cdr! sub-table val)
                      (iter (cdr key-iter) sub-table))
                  (set-cdr!
                    table
                    (cons 
                      (make-subtable key-iter)
                      (cdr table)))))))
      (define (make-subtable key-iter)
        (if (null? (cdr key-iter))
            (cons (car key-iter) val)
            (list (car key-iter) (make-subtable (cdr key-iter)))))
      (iter key local-table))
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc)  lookup)
        ((eq? m 'insert-proc!) insert!)
        (else
          (error "Unknown operation -- TABLE" m))))
  dispatch))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(define p print)

; 1-d
(define table1 (make-table))
(define lookup1 (table1 'lookup-proc))
(define insert1! (table1 'insert-proc!))
(insert1! '(a) 1)
(p (lookup1 '(a))) ; 1
(insert1! '(b) 2)
(insert1! '(c) 3)
(insert1! '(a) 10)
(p (lookup1 '(a))) ; 10
(p (lookup1 '(b))) ; 2
(p (lookup1 '(c))) ; 3
(p "end")
; 3-d
(define table3 (make-table))
(define lookup3 (table3 'lookup-proc))
(define insert3! (table3 'insert-proc!))
(insert3! '(a x i) 11)
(p (lookup3 '(a x i))) ; 1
(insert3! '(b y k) 22)
(p (lookup3 '(b y k))) ; 22
(insert3! '(c z l) 33)
(insert3! '(b y k) 99)
(p (lookup3 '(b y k))) ; 99
(p (lookup3 '(c y k))) ; #f

; END