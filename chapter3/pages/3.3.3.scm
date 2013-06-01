;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.3 Representing Tables
;;

; One-dimensional tables ; 

(define (lookup key table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond
    ((null? records) 
     #f)
    ((equal? key (caar records)) 
     (car records))
    (else
      (assoc key (cdr records)))))

(define (insert! key value table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

; Two-dimensional tables ; 

(define (lookup key-1 key-2 table)
  (let 
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let 
          ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let 
    ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let 
          ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

; Creating local tables ; 

(define (make-table)
  (let
    ((local-table (list '*table*)))
    (define (lookup key-1 key-2 table)
      (let 
        ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let 
              ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value table)
      (let 
        ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let 
              ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc)  lookup)
        ((eq? m 'insert-proc!) insert!)
        (else
          (error "Unknown table"))))
    dispatch))

; END