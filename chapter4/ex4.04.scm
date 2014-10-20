;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.04.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./" :relative)
(load "ex4.03.scm")

(define (eval-and? exp) (tagged-list? exp 'and))

(define (eval-or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (eval-and-iter exp rest)
    (if (null? exp)
        #f
        (let ((first-exp (eval (car exp) env))
              (rest-exp (cdr exp)))
          (if (true? first-exp)
              (eval-and-iter rest-exp first-exp)
              #f))))
  (if (null? exp)
      #t
      (eval-and-iter exp rest)))

(define (eval-or exp env)
  (if (null? exp)
      #f
      (let ((first-exp (car exp))
            (rest-exp (cdr exp)))
        (if (true? first-exp)
            first-exp
            (eval-or rest-exp env)))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (and #t #f))  ;; => #f
(print (and #t #t))  ;; => #t
(print (or #t #f))  ;; => #t
(print (or #t #t))  ;; => #t
(print (or #f #f))  ;; => #f
