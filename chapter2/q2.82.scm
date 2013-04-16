;; @author naoiwata
;; SICP Chapter2
;; question 2.82

(define (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    (define (do-coerce x type)
      (let
        ((coercion (get-coercion (type-tag x) type)))
        (and coercion (coercion x))))
    (define (find-proc-coerceds op args types)
      (if (null? types)
          #f
          (let
            ((type (car types)))
            (let
              ((coerceds (map (lambda (x) (do-coerce x type)) args)))
              (if coerceds
                  (let 
                    ((proc (get op (map type-tag coerceds))))
                    (if proc
                        (cons proc coerceds)
                        (find-proc-coerceds op args (cdr types))))
                  (find-proc-coerceds op args (cdr types)))))))
    (let
      ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((proc-coerceds (find-proc-coerceds op args type-tags)))
            (if proc-coerceds
                (apply (car proc-coerceds) (map contents (cdr proc-coerceds)))
                (error "No method for these types -- APPLY-GENERIC"
                       (list op type-tags))))))))

;  -----------------------------------------------------------------
; test
; ------------------------------------------------------------------

; utility settings -------------------------------------------------

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (else
      "BAD TAGGED DATUM -- TYPE-TAG" datum)))

(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else
      "BAD TAGGED DATUM -- CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (gcd a b)
  (if (= b 0)
      a
    (gcd b (remainder a b))))

(define (square x) (* x x))

; put and get procedures -------------------------------------------

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; ordinary procedure -----------------------------------------------

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational procedure ----------------------------------------------

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
       (= (* (numer x) (denon y))
          (* (denom x) (numer y))))
  (define (=zero? x)
    (zero? (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; rectangular procedure ------------------------------------------

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-imag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-imag-ang 'rectangular
       (lambda (r a) (tag (make-from-imag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

; polar procedure ------------------------------------------------

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-imag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-imag-ang 'polar
       (lambda (r a) (tag (make-from-imag-ang r a))))
  'done)

(define (make-from-imag-ang r a)
  ((get 'make-from-imag-ang 'polar) r a))

; complex procedure ---------------------------------------------

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-imag-ang r a)
    ((get 'make-from-imag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-imag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-imag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (define (=zero? x)
    (and (zero? (real-part x)) 
         (zero? (imag-part x))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-imag-ang 'complex
       (lambda (r a) (tag (make-from-imag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (eq? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z1) (=zero? z1)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-imag-ang 'complex) r a))

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package) 
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x)     (apply-generic 'angle x))
(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (equ? x)      (apply-generic 'equ? x))
(define (=zero? x)    (apply-generic '=zero? x))

;; test ---------------------------------------------------

; define coercion procedures
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(define (scheme-number->scheme-number n) 
  n)
(define (complex->complex z) 
  z)

; table for coercion
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

; resister table
(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; test value
(define n1 (make-scheme-number 1))
(define z2 (make-complex-from-real-imag 2 3))
(print n1 z2) ;(scheme-number . 1)(complex rectangular 2 . 3)

(print (add n1 z2)) ; (complex rectangular 3 . 3)

; END