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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; TODO
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedure))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-inunderlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

(define (deriver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-parameters
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(deriver-loop)
