;; @author naoiwata
;; SICP Chapter2
;; question 2.75

(define (make-from-mag-imag r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

; END