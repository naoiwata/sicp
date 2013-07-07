;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.26.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-table)
  (define (key record) (car record))
  (define (node tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree node left right)
    (list node left right))
  (define (adjoin-set-node x set)
    (cond 
      ((null? set) (make-tree x '() '()))
      ((= (key x) (key (node set))) set)
      ((< (key x) (key (node set)))
       (make-tree (node set)
                  (adjoin-set-node x (left-branch set))
                  (right-branch set)))
      ((> (key x) (key (node set)))
       (make-tree (node set)
                  (left-branch set)
                  (adjoin-set-node x (right-branch set))))))
  (define (lookup-tree given-key set-of-records)
    (if (null? set-of-records)
        #f
        (let 
          ((record (node set-of-records)))
          (cond ((= given-key (key record)) record)
                ((< given-key (key record))
                 (lookup-tree given-key (left-branch set-of-records)))
                ((> given-key (key record))
                 (lookup-tree given-key (right-branch set-of-records)))))))
  (let
    ((local-table (list '*table*)))
    (define (lookup key)
      (define (iter key-iter table)
        (if (null? key-iter)
            #f
            (let
              ((sub-table (lookup-tree (car key-iter) (cdr table))))
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
              ((sub-table (lookup-tree (car key-iter) (cdr table))))
              (if sub-table
                  (if (null? (cdr key-iter))
                      (set-cdr! sub-table val)
                      (iter (cdr key-iter) sub-table))
                  (set-cdr! table
                            (adjoin-set-node (make-subtable key-iter)
                                             (cdr table)))))))
      (define (make-subtable key-iter)
        (if (null? (cdr key-iter))
            (cons (car key-iter) val)
            (cons (car key-iter) 
                  (make-tree 
                    (make-subtable (cdr key-iter)) '() '()))))
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
(insert1! '(1) 'a)
(p (lookup1 '(1))) ; 1
(insert1! '(2) 'b)
(insert1! '(3) 'c)
(insert1! '(1) 'z)
(p (lookup1 '(1))) ; 10
(p (lookup1 '(2))) ; 2
(p (lookup1 '(3))) ; 3

; 3-d
(define table3 (make-table))
(define lookup3 (table3 'lookup-proc))
(define insert3! (table3 'insert-proc!))
(insert3! '(1 11 111) 'a)
(p (lookup3 '(1 11 111))) ; a
(insert3! '(2 22 222) 'b)
(p (lookup3 '(2 22 222))) ; b
(insert3! '(3 33 333) 'c)
(insert3! '(2 22 222) 'z)
(p (lookup3 '(2 22 222))) ; z
(p (lookup3 '(3 33 444))) ; #f

; END