;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.5.
;;

(define (random x)
  (* x (random-real)))

(define (random-in-range low high)
  (let
    ((range (- high low)))
    (+ low (random range))))

(define (monte-calro trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0)
       (/ trials-passed trials))
      ((experiment)
       (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else
        (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; probability * area
(define (estimate-integral p x1 x2 y1 y2 count)
  (*
    ; probability
    (monte-calro
      count
      (lambda ()
        (p
          (random-in-range x1 x2)
          (random-in-range y1 y2))))
    ; area
    (- x2 x1)
    (- y2 y1)))

; (x - a)^2 + (y - b)^2 = r^2
(define (area a b r)
  (lambda (x y)
    (<= (+ (square (- x a))
         (square (- y b)))
      (square r))))

(define (square k) (* k k))

; test
(use srfi-27)
(define exp (area 5 7 3))

(print (exact->inexact (estimate-integral exp 2 8 4 10 10000))) ; 28.278

; END