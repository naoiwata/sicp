;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 4.1.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

; left->right
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let
        ((first-eval (eval (first-operand exps) env)))
        (cons
          first-eval
          (list-of-values-l2r (rest-operands exps) env)))))

; right->left
(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let
        ((first-eval (list-of-values-r2l (rest-operands exps) env)))
        (cons
          (eval (first-operand exps) env)
          first-eval))))

; END