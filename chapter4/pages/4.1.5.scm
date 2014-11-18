;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.5 Data as Programs
;;

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1) n))))

