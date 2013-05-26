;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.1 Mutable List Structure
;;
#|
(define (cons x y)
  (let
    ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)))
|#

;; Sharing and identity ;;

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))

; test
(print z1) ; ((a b) a b)
(print (set-to-wow! z1)) ; ((wow b) wow b)
(print (eq? (car z1) (cdr z1))) ; #t

(define z2 (cons (list 'a 'b) (list 'a 'b)))

; test
(print z2) ; ((a b) a b)
(print (set-to-wow! z2)) ; ((wow b) a b)
(print (eq? (car z2) (cdr z2))) ; #f

;; Mutation is just assignment ;;
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

; END