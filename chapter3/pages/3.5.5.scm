;;
;; @author naoiwata
;; SICP Chapter3
;; 3.5.5 Modularity of Functional Programs and Modularity of Objects
;;

(define rand
  (let
    ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-numbers
  (cons-stream
    random-init
    (stream-map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs 
    (lambda (r1 r2) (= (gcd r1 r2) 1))
    random-numbers))

(define (map-successive-pairs f s)
  (f (strem-car s) (strem-car (strem-cdr s)))
  (map-successive-pairs f (strem-cdr (strem-cdr s))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (/ passed (+ passed failed))
    (monte-carlo
      (strem-cdr experiment-stream) passed failed))
  (if (strem-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
    (lambda (p) (sqrt (/ 6 p)))
    (monte-carlo cesaro-stream 0 0)))

