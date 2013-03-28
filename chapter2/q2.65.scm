;; @author naoiwata
;; SICP Chapter2
;; question 2.65

(add-load-path "." :relative)
(load "pages/2.3.3.scm") ; intersection-set
(load "q2.63.scm") ; tree->list
(load "q2.64.scm") ; list->tree

(define (union-set-iter set1 set2)
  (cond
    ((null? set1)
     set2)
    ((null? set2)
     set1)
    (else
      (let
        ((x1 (car set1)) (x2 (car set2)))
        (cond
          ((= x1 x2)
           (cons x1 (union-set (cdr set1) (cdr set2))))
          ((< x1 x2)
           (cons x1 (union-set (cdr set1) set2)))
          ((< x2 x1)
           (cons x2 (union-set set1 (cdr set2)))))))))

(define (intersection-set-iter set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let 
        ((x1 (car set1)) (x2 (car set2)))
        (cond
          ((= x1 x2)
           (cons
             x1
             (intersection-set (cdr set1) (cdr set2))))
          ((< x1 x2)
           (intersection-set (cdr set1) set2))
          ((< x2 x1)
           (intersection-set set1 (cdr set2)))))))

(define (make-tree f tree-a tree-b)
  (let
    ((list-a (tree->list-2 tree-a))
     (list-b (tree->list-2 tree-b)))
    (list->tree (f list-a list-b))))

(define (union-set tree-a tree-b)
  (make-tree union-set-iter tree-a tree-b))

(define (intersection-set tree-a tree-b)
  (make-tree intersection-set-iter tree-a tree-b))

; END