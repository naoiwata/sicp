;; @author naoiwata
;; SICP Chapter2
;; question 2.64

(add-load-path "." :relative)
(load "pages/2.3.3.scm")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let 
        ((left-size (quotient (- n 1) 2)))
        (let 
          ((left-result (partial-tree elts left-size)))
          (let 
            ((left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1))))
            (let 
              ((this-entry (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let 
                ((right-tree (car right-result))
                 (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;; (a)

; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
#|
  5
  |
  +---+
  |   |
  1   9
  |   |
  3   +---+
      |   |
      7   11
(list->tree (list 1 3 5 7 9 11 13))
; see detail of this procedure :)

;; n=0: (partial-tree '(1 3 5 7 9 11) 0)
(partial-tree '(1 3 5 7 9 11) 0)
  |
  V
'(() 1 3 5 7 9 11)

;; n=1: (partial-tree '(1 3 5 7 9 11) 1)
(partial-tree '(1 3 5 7 9 11) 1)
  |
  V
(cons (make-tree 
        (cadr (partial-tree '(1 3 5 7 9 11) 0)) 
        (car (partial-tree '(1 3 5 7 9 11) 0)) 
        (car (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 0))) 
      (cdr (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 0)))
  |
  V
(cons (make-tree 
        (cadr '(() 1 3 5 7 9 11)) 
        (car '(() 1 3 5 7 9 11)) 
        (car (partial-tree (cdr (cdr '(() 1 3 5 7 9 11))) 0))) 
      (cdr (partial-tree (cdr (cdr '(() 1 3 5 7 9 11))) 0)))
  |
  V
(cons (make-tree 
        1 
        '() 
        '() 
      '(3 5 7 9 11)))
  |
  V
'((1 () ()) 3 5 7 9 11)
 
;; n=2: (partial-tree '(1 3 5 7 9 11) 2)
(partial-tree '(1 3 5 7 9 11) 2)
  |
  V
(cons (make-tree 
        (cadr (partial-tree '(1 3 5 7 9 11) 0)) 
        (car (partial-tree '(1 3 5 7 9 11) 0)) 
        (car (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 1))) 
      (cdr (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 0))) 1)))
  |
  V
(cons (make-tree 
        1
        '()
        (car (partial-tree '(3 5 7 9 11) 1))) 
      (cdr (partial-tree '(3 5 7 9 11) 1)))
  |
  V
(cons (make-tree 
        1
        '()
        (car '((3 () ()) 5 7 9 11)) 
      (cdr '((3 () ()) 5 7 9 11))
  |
  V
(cons (make-tree 
        1
        '()
        '(3 () ()) 
      (5 7 9 11)))
  |
  V
'((1 () (3 () ())) 5 7 9 11)    

;; n=3: (partial-tree '(1 3 5 7 9 11) 3)
(partial-tree '(1 3 5 7 9 11) 3)
  |
  V
(cons (make-tree 
        (cadr (partial-tree '(1 3 5 7 9 11) 1)) 
        (car (partial-tree '(1 3 5 7 9 11) 1)) 
        (car (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 1))) 1))) 
      (cdr (partial-tree (cdr (cdr (partial-tree '(1 3 5 7 9 11) 1))) 1))))
  |
  V
(cons (make-tree 
        (cadr '((1 () ()) 3 5 7 9 11)) 
        (car '((1 () ()) 3 5 7 9 11)) 
        (car (partial-tree (cdr (cdr '((1 () ()) 3 5 7 9 11))) 1))) 
      (cdr (partial-tree (cdr (cdr '((1 () ()) 3 5 7 9 11))) 1))))
  |
  V
(cons (make-tree 
        3
        '(1 () ())
        (car (partial-tree '(5 7 9 11) 1))) 
      (cdr (partial-tree '(5 7 9 11) 1)))
  |
  V
(cons (make-tree 
        3
        '(1 () ())
        (car '((5 () ()) 7 9 11)))
      (cdr '((5 () ()) 7 9 11)))
  |
  V
(cons (make-tree 
        3
        '(1 () ())
        '(5 () ())) 
      '(7 9 11))
  |
  V
'((3 (1 () ()) (5 () ())) 7 9 11)
 
;; n=4: (partial-tree '(1 3 5 7 9 11) 4)
'((3 (1 () ()) (5 () (7 () ()))) 9 11)

;; n=5: (partial-tree '(1 3 5 7 9 11) 5)
'((5 (1 () (3 () ())) (7 () (9 () ()))) 11)

;; n=6: (partial-tree '(1 3 5 7 9 11) 6)
'((5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))

;; (b)
the number of steps: Θ(n)
|#

; test
(print (list->tree (list 1 3 5 7 9 11)))
; (7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (13 () ())))
(print (list->tree (list 1 3 5 7 9 11 13 15)))
; (7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (13 () (15 () ()))))

; END