;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.8.
;;

; (+ (f 0) (f 1)) -> 0
; (+ (f 1) (f 0)) -> 1

(define f
  (let
    ((x 0))
    (define (dispatch m)
      (let
        ((y x))
        (begin
          (set! x m)
          y)))
    dispatch))

(f 0)
(f 1)
; (+ (f 0) (f 1)) -> 0

(f 1)
(f 0)
; (+ (f 1) (f 0)) -> 1 

; END