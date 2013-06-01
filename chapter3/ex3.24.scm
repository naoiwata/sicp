;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.24.
;;

(define (make-table same-key?)
  (define (assoc-same same-key? key records)
    (cond
      ((null? records) 
       #f)
      ((same-key? key (caar records)) 
       (car records))
      (else
        (assoc-same same-key? key (cdr records)))))
  (let
    ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let 
        ((subtable (assoc-same same-key? key-1 (cdr local-table))))
        (if subtable
            (let 
              ((record (assoc-same same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let 
        ((subtable (assoc-same same-key? key-1 (cdr local-table))))
        (if subtable
            (let 
              ((record (assoc-same same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc)  lookup)
        ((eq? m 'insert-proc!) insert!)
        (else
          (error "Unknown table"))))
    dispatch))

; test
(define q (make-table equal?))
(define lookup (q 'lookup-proc))
(define insert! (q 'insert-proc!))

(insert! 'a '+ 11)
(insert! 'a '- 22)
(insert! 'b '2 33)
(insert! 'c '(5) 44)

(print (lookup 'a '+)); 11
(print (lookup 'a '-)) ; 22
(print (lookup 'b (+ 1 1))) ; 33
(print (lookup 'c '(5))) ; 44

; memo
(eq? '(3) '(3)) ; #f
(equal? '(3) '(3)) ; #t

; END