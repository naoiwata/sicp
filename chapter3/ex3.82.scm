;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.82.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (random x)
  (* x (random-real)))

(define (random-in-range low high)
  (define rand
    (let ((range (- high low)))
      (+ low (* random range))))
  (cons-stream rand (random-in-range low high)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream)
                              passed
                              failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1)))) 

(define (estimate-integral p x1 x2 y1 y2)
  (stream-map
    (lambda (i) (* (* (- x2 x1) (- y2 y1)) i 1.0))
    (monte-carlo (stream-map
                   (lambda (x y) (p x y))
                   (random-numbers-in-range x1 x2)
                   (random-numbers-in-range y1 y2))
                 0 0)))
