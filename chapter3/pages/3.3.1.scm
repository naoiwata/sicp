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

; END