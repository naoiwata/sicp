;; @author naoiwata
;; SICP Chapter2
;; question 2.78

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) datum)
    (else
      (error "BAD TAGGED DATUM -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) datum)
    (else
      (error "BAD TAGGED DATUM -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

; END