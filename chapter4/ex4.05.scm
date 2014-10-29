;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.05.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define (sequence->exp seq)
  (cond 
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else
      (make-begin seq))))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (let ((actions (cond-actions first))
                           (predicate (cond-predicate first)))
                       (if (eq? (car actions) '=>)
                           (list (cadr actions) predicate)
                           (sequence->exp actions)))
                     (expand-clauses rest))))))
