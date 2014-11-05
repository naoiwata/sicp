;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.3  Evaluator Data Structures
;;

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? (x #f)))
