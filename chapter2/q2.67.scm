;; @author naoiwata
;; SICP Chapter2
;; question 2.67

(add-load-path "." :relative)
(load "pages/2.3.4.scm")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(print (decode sample-message sample-tree))
; (A D A B B C A)

; END
