;; @author naoiwata
;; SICP Chapter2
;; question 2.65

(add-load-path "." :relative)
(load "pages/2.3.3.scm") ; intersection-set
(load "q2.62.scm") ; union-set
(load "q2.63.scm") ; tree->list
(load "q2.64.scm") ; list->tree

(define (make-tree f tree-a tree-b)
  (let
    ((list-a (tree->list-2 tree-a))
     (list-b (tree->list-2 tree-b)))
    (list->tree (f list-a list-b))))

(define (union-set-tree tree-a tree-b)
  (make-tree union-set tree-a tree-b))

(define (intersection-set-tree tree-a tree-b)
  (make-tree intersection-set tree-a tree-b))

; END