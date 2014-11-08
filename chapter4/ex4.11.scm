;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.11.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (cdar frame))
            (else
             (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))
