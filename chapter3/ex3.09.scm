;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.9.
;;

(define (fanctional n)
  (if (= n 1)
      1
      (* n (fanctional (- n 1)))))

; (fanctional 6)
; n:6 -> n:5 -> n:4 -> n:3 -> n:2 -> n:1 

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; (fact-iter 1 1 6)
; product:1   -> product:1   -> product:2   -> product:6  -> product:24   -> product:120 -> product:720
; counter:1   -> counter:2   -> counter:3   -> counter:4   -> counter:5   -> counter:6   -> counter:7
; max-count:6 -> max-count:6 -> max-count:6 -> max-count:6 -> max-count:6 -> max-count:6 -> max-count:6

(print (fanctional 6)) ; 720