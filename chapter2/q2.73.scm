;; @author naoiwata
;; SICP Chapter2
;; question 2.73

;(add-load-path "." :relative)
;(load "pages/2.3.2.scm")

(define (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp) 
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp))))
    ;; <more rules can be added here>
    (else 
      (error "unknown expression type -- DERIV" exp))))

; redefine
(define (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp) 
     (if (same-variable? exp var) 1 0))
    (else
      ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) 
  (car exp))

(define (operands exp) 
  (cdr exp))

; (a)
#|
1. 上でやったことを説明せよ。
汎用化できる微分式を、指定した型タグを適用した式に引数を取得して評価させている。

2. number? variable?がデータ主導の振り分けに入れられないのはなぜか。
これらは型タグを持たないなので、データ型タグによる式判別ができない為。
|#

; (b)
;; lib
(define (variable? x)
  (symbol? x))

;; sum
(define (install-addtion-package)
  ;; internal procedures
  ; selector
  (define (addend s)
    (cadr s))
  (define (augend s)
    (caddr s))
  ; element
  (define (make-sum a1 a2)
    (cond
      ((=number? a1 0)
       a2)
      ((=number? a2 0)
       a1)
      ((and (number? a1) (number? a2))
       (+ a1 a2))
      (else
        (list '+ a1 a2))))
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; interface
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

;; product
(define (install-addtion-package)
  ;; internal procedures
  ; selector
  (define (multiplier s)
    (cadr s))
  (define (multiplicand s)
    (caddr s))
  ; element
  (define (make-product m1 m2)
    (cond 
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list '* m1 m2))))
  (define (product-deriv exp var)
    ((get 'make-sum '+)
     (make-product
       (multiplier exp)
       (deriv (multiplicand exp) var))
     (make-product 
       (deriv (multiplier exp) var)
       (multiplicand exp))))
  ;; interface
  (put 'make-product '* make-product)
  (put 'deriv '* product-deriv)
  'done)

; (c)
  
; (d)