;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.16.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) *unassigned*)
                 (error "unassigned variable" var)
                 (car vals)))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
