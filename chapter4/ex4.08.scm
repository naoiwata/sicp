;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.08.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (named-let? exp)
  (and  (tagged-list? exp 'let)
        (symbol? (cadr exp))))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-variables exp)
  (caddr exp))

(define (named-let-body exp)
  (cdddr exp))

(define (let-local-variables exp)
  (map car (let-variables exp)))

(define (let-local-expressions exp)
  (map cadr (let-variables exp)))

(define (sequence->exp seq)
  (cond
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else
     (make-begin seq))))

(define (let*->combination exp)
  (if (named-let? exp)
      (sequence->exp
       (lit (cons 'define (cons (cons (named-let-name exp) (named-let-variables exp))))
            (list (named-let-body exp)
                  ;;tTODO
                  )))
      (if (let-variables exp)
          '()
          (cons (make-lambda (let-local-variables exp)
                             (let-body exp))
                (let-local-expressions exp)))))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-rparameters exp)
                     (lambda-body exp)
                     env))
    ((let? exp)
     (eval (let*->combination exp) env))
    ((begin? exp)
     (eval-sequence (lambda-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    (else
      (error "Unknown expression type -- EVAL" exp))))
