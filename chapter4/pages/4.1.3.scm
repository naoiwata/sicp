;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.3  Evaluator Data Structures
;;

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? (x #f)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))
