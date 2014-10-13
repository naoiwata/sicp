;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.01.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (no-operands? x) (null? x))
(define first-operand car)
(define rest-operands cdr)


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-from-right exps env)
  (if (no-operands? exps)
      '()
      (let
        ((rest-of-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-of-values))))

(define (list-of-values-from-left exps env)
  (if (no-operands? exps)
      '()
      (let
        ((first-of-values (list-of-values (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------
