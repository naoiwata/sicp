;; @author naoiwata
;; SICP Chapter2
;; question 2.69

(add-load-path "." :relative)
(load "pages/2.3.4.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
        (adjoin-set
          (make-code-tree (car pairs) (cadr pairs))
          (cddr pairs)))))
; test
(define pairs '((A 4) (B 2) (C 1) (D 1)))
(print (generate-huffman-tree pairs))
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

#|
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
|
V
(successive-merge (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))                  
|
V
(successive-merge '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)))
|
V
(successive-merge
        (adjoin-set
          (make-code-tree '(leaf D 1) '(leaf C 1))
          '((leaf B 2) (leaf A 4))))
|
V
(successive-merge
        (adjoin-set
          '((leaf D 1) (leaf C 1) (D C) 2)
          '((leaf B 2) (leaf A 4))))
|
V
(successive-merge
        '((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (leaf A 4)))
|
V
(successive-merge
        (adjoin-set
          (make-code-tree '(leaf B 2) '((leaf D 1) (leaf C 1) (D C) 2))
          '((leaf A 4))))
|
V
(successive-merge
        (adjoin-set
          '((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
          '((leaf A 4))))
|
V
(successive-merge
        '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)))
|
V
(successive-merge
  		(adjoin-set
          (make-code-tree '(leaf A 4) '((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4))
          null))
|
V
(successive-merge
  		(adjoin-set
          '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
          null))
|
V
'((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
|#

; END