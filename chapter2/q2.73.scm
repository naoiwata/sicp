;; @author naoiwata
;; SICP Chapter2
;; question 2.73

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
      ((get 'deriv-table (operator exp)) (operands exp) var))))

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

; (b), (c)
;; lib
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; hash table
(define deriv-table (make-hash-table))

(define (put op type item)
  (hash-table-put! deriv-table type item))

(define (get op type)
  (hash-table-get deriv-table type))

; deriv
(define (install-deriv-package)
  ;;; internal procedures
  ;; sum
  ; selector
  (define (addend s)
    (car s))
  (define (augend s)
    (cadr s))
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
  ;; product
  ; selector
  (define (multiplier s)
    (car s))
  (define (multiplicand s)
    (cadr s))
  ; element
  (define (make-product m1 m2)
    (cond 
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list '* m1 m2))))
  (define (product-deriv exp var)
    (make-sum
     (make-product
       (multiplier exp)
       (deriv (multiplicand exp) var))
     (make-product 
       (deriv (multiplier exp) var)
       (multiplicand exp))))
  ;; exponent
  ; selector
  (define (base s)
    (car s))
  (define (exponent s)
    (cadr s))
  ; element
  (define (make-exponent x a)
    (cond
      ((=number? a 0) 1)
      ((=number? a 1) x)
      (else
        (list '** x a))))
  (define (exponent-deriv exp var)
    (make-product
      (exponent exp)
      (make-product
        (make-exponent (base exp) (make-sum (exponent exp) -1))
        (deriv (base exp) var))))
  ;;; interface
  (put 'sum-deriv '+ sum-deriv)
  (put 'product-deriv '* product-deriv)
  (put 'exponent-deriv '** exponent-deriv)
  'done)

; test
(install-deriv-package)
(print (deriv '(+ x 3) 'x)) ; 1
(print (deriv '(* x y) 'x)) ; y
(print (deriv '(** x 4) 'x)) ; (* 4 (** x 3))
(print (deriv '(* (* x y) (+ x 3)) 'x)) ; (+ (* x y) (* y (+ x 3)))

; (d)
#|
(get <op> <type>)
(put <op> <type> <item>)

getの第一引数と第二引数を入れ替えた場合同様にputの第一引数と第二引数のみを変更すれば良い
(get <type> <op>)
(put <type> <op> <item>)
|#

; END