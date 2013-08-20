;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.62.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (div-series s1 s2)
    (if (= (stream-car s2) 0)
        (error "division is zero :(")
        (mul-series
          s1
          (scale-stream (invert-unit-series s2)
                        (/ 1 (stream-car s2))))))

(define tangent-series
  (div-series sine-series cosine-series))

; END