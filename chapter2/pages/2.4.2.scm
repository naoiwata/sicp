;; @author naoiwata
;; SICP Chapter2
;; 2.4.2 Tagged data

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "BAD TAGGED DATUM -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (car datum)
      (error "BAD TAGGED DATUM -- CONTENTS" datum)))

(define (rectanguler? z)
  (eq? (type-tag z) 'rectanguler))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; Ben
(define (real-part-rectangular z) 
  (car z))

(define (imag-part-rectangular z) 
  (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; Allyssa
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) 
  (car z))

(define (angle-polar z) 
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; Tagged
(define (real-part z)
  (cond 
    ((rectangular? z)
     (real-part-rectangular (contents z)))
    ((polar? z)
     (real-part-polar (contents z)))
    (else 
      (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond 
    ((rectangular? z)
     (imag-part-rectangular (contents z)))
    ((polar? z)
     (imag-part-polar (contents z)))
    (else 
      (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond 
    ((rectangular? z)
     (magnitude-rectangular (contents z)))
    ((polar? z)
     (magnitude-polar (contents z)))
    (else 
      (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond 
    ((rectangular? z)
     (angle-rectangular (contents z)))
    ((polar? z)
     (angle-polar (contents z)))
    (else 
      (error "Unknown type -- ANGLE" z))))

; use these type tagged data
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; END