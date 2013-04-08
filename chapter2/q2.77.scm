;; @author naoiwata
;; SICP Chapter2
;; question 2.77

; utility settings -------------------------------------------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    (let 
      ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (gcd a b)
  (if (= b 0)
      a
	  (gcd b (remainder a b))))

(define (square x) (* x x))

; put and get procedures -------------------------------------------

#|
(define operation-table (make-hash-table))

(define (put op type item)
  (if
    (hash-table-exists? operation-table type)
    (let
      ((type-table (hash-table-get operation-table type)))
      (hash-table-put! type-table op item))
    (let
      ((new-type-table (make-hash-table)))
      (hash-table-put! new-type-table op item)
      (hash-table-put! operation-table type new-type-table))))

(define (get op type)
  (if
    (hash-table-exists? operation-table type)
    (let
      ((type-table (hash-table-get operation-table type)))
      (hash-table-get type-table op))
    #f))
|#

; thanks to -> http://karetta.jp/book-node/YetAnotherSICPAnswerBook/251301
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
  ;; ------- add from -------
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; ------- add end -------
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
  ;; ------- add from -------
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  ;; ------- add end --------
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-imag-ang 'complex) r a))

; test --------------------------------------------------------

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package) 
(install-rational-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; test
(define z 
  (make-complex-from-real-imag 3 4))
; -> (complex rectangular 3 . 4)

(define (magnitude x) 
  (apply-generic 'magnitude x))

(magnitude z)
; 5

#|
(magnitude z) 
->
(magnitude (make-complex-from-real-imag 3 4))
->
(magnitude ((get 'make-from-real-imag 'complex) 3 4))
->
(magnitude ((lambda (x y) (tag (make-from-real-imag x y))) 3 4))
->
(magnitude ((lambda (x y) (cons 'complex (make-from-real-imag x y))) 3 4))
->
(magnitude ((lambda (x y) (cons 'complex (cons 'rectangular (cons x y)))) 3 4))
->
(magnitude (cons 'complex (cons 'rectangular (cons 3 4))))
->
(apply-generic 'magnitude (cons 'complex (cons 'rectangular (cons 3 4))))
->
  (let 
    ((type-tags (map type-tag (cons 'complex (cons 'rectangular (cons 3 4))))))
    (let 
      ((proc (get 'magnitude type-tags)))
      (if proc
          (apply proc (map contents (cons 'complex (cons 'rectangular (cons 3 4)))))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list 'magnitude type-tags)))))
->
  (let 
    ((type-tags '(complex)))
    (let 
      ((proc (get 'magnitude type-tags)))
      (if proc
          (apply proc (cons 'rectangular (cons 3 4)))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list 'magnitude type-tags)))))
->
(apply
  (get 'magnitude '(complex))
  (cons 'rectangular (cons 3 4)))
->
(apply-generic 'magnitude (cons 'rectangular (cons 3 4)))
->
  (let 
    ((type-tags (map type-tag (cons 'rectangular (cons 3 4)))))
    (let 
      ((proc (get 'magnitude type-tags)))
      (if proc
          (apply proc (map contents (cons 'rectangular (cons 3 4))))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list 'magnitude type-tags)))))
->
(apply 
  (get 'magnitude (map type-tag (cons 'rectangular (cons 3 4))))
  (map contents (cons 'rectangular (cons 3 4))))
->
(apply
  (get 'magnitude '(rectangular))
  '(3 4))
->
(apply
  (lambda (z) 
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))) )
  '(3 4))
->
(sqrt (+ (square (real-part (3 4)))
         (square (imag-part (3 4)))))
->
(sqrt (+ (square (car (3 4)))
         (square (cdr (3 4)))))  
->
(sqrt (+ (square 3)
         (square 4))
->
(sqrt (+ 9
         16)  
->
(sqrt 25)
-> 5

-> 'apply-generic' is used twice:)
|#

; END