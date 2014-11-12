;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.4  Running the Evaluator as a Program
;;

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! '#t #t initial-env)
    (define-variable! '#f #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

