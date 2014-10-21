;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.05.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-if-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (else
                  (error "ELSE clause isn't last -- COND->IF" clauses)))
            (make-if (cond-predicate first)
                     (let ((actions (cond-actions first))
                           (predicate (cond-predicate first)))
                       (if (car actions '=>)
                           (list (cadr actions) predicate)
                           (sequence->exp actions)))
                     (expand-clauses rest))))))
