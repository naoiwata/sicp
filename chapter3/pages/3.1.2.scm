;;
;; @author naoiwata
;; SICP Chapter3
;; 3.1.2 The Benefits of Introducing Assignment
;;

(define rand
  (let
    ((x random-init))
    (lambda ()
      (set! x (rand-update x)))
    x))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-calro trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

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

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let
      ((x1 (rand-update x)))
      (let
        ((x2 (rand-update x1)))
        (cond
          ((= trials-remaining 0)
           (/ trials-passed trials))
          ((= (gcd x1 x2) 1)
           (iter (- trials-remaining 1)
                 (- trials-passed 1)
                 x2))
          (else
            (iter (- trials-remaining 1)
                  trials-passed
                  x2))))))
  (iter trials 0 initial-x))
