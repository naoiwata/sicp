;; @author naoiwata
;; SICP Chapter2
;; question 2.63

(add-load-path "." :relative)
(load "pages/2.3.3.scm")

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
        (tree->list-1 (left-branch tree))
        (cons 
          (entry tree)
          (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
          (left-branch tree)
          (cons 
            (entry tree)
            (copy-to-list 
              (right-branch tree)
              result-list)))))
  (copy-to-list tree '()))

; (a)
; test
(define tree-a '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(print (tree->list-1 tree-a)) ; (1 3 5 7 9 11)
(print (tree->list-2 tree-a)) ; (1 3 5 7 9 11)

(define tree-b '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 
(print (tree->list-1 tree-b)) ; (1 3 5 7 9 11)
(print (tree->list-2 tree-b)) ; (1 3 5 7 9 11)

(define tree-b '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(print (tree->list-1 tree-b)) ; (1 3 5 7 9 11)
(print (tree->list-2 tree-b)) ; (1 3 5 7 9 11)
; these results are same.

; (b)
#|
tree->list-1 
	method: Linear Recursion
	step:   log(n) * append procedure = log(n) * n
tree->list-2 
	method: Iteration
	step:   log(n)
|#

; END