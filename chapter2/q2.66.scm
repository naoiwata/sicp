;; @author naoiwata
;; SICP Chapter2
;; question 2.66

(add-load-path "." :relative)
(load "pages/2.3.3.scm")
(load "q2.64.scm")

#| 
guess this step,
-> make record '((1 apple) (2 banana) (3 cat) (4 dog) (5 egg))
	-> make-tree
		-> lookup
|#

(define (key record) (car record))

(define (value record) (cdr record))

(define (make-record key value)
  (cons key value))

(define (lookup-iter given-key set-of-records)  
  (if
    (null? set-of-records)
     #f
    (let
      ((entry-record (entry set-of-records)))
      (let
        ((key-record (key entry-record)))
        (cond
          ((= given-key key-record)
           (value entry-record))
          ((< given-key key-record)
           (lookup-iter
             given-key (left-branch set-of-records)))
          ((< key-record value)
           (lookup-iter
             given-key (right-branch set-of-records)))
          (else
            (display "ERROR")))))))

(define (lookup given-key set-of-records)
  (let
    ((record-tree (list->tree set-of-records)))
    (lookup-iter given-key record-tree)))

(define record 
(list
  (make-record 1 'apple) 
  (make-record 2 'banana) 
  (make-record 3 'cat) 
  (make-record 4 'dog) 
  (make-record 5 'egg)))

; test
(print (lookup 1 record)) ; apple
(print (lookup 3 record)) ; cat

; END
