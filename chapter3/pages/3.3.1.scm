;;
;; @author naoiwata
;; SICP Chapter3
;; 3.3.1 Mutable List Structure
;;

(define (cons x y)
  (let
    ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)))

;; Sharing and identity ;;
