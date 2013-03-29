;; @author naoiwata
;; SICP Chapter2
;; question 2.68

(add-load-path "." :relative)
(load "q2.67.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append
        (encode-symbol (car message) tree)
        (encode (cdr message) tree))))

; test
(print sample-tree)
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(define (encode-symbol symbol tree)
  (if (leaf? tree)
     '()
   (cond
      ((contained? symbol (symbols (left-branch tree)))
       (cons
         0
         (encode-symbol symbol (left-branch tree))))
      ((contained? symbol (symbols (right-branch tree)))
       (cons
         1
         (encode-symbol symbol (right-branch tree))))
      (else
        (error "not found tree")))))

(define (contained? symbol tree)
  (cond
    ((null? tree)
     #f)
    ((eq? symbol (car tree))
     #t)
    (else
      (contained? symbol (cdr tree)))))

; test
(print (encode '(A D A B B C A) sample-tree))
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; END