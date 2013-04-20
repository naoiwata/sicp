;; @author naoiwata
;; SICP Chapter2
;; question 2.87

; utility settings -------------------------------------------------

; define types-tower list.
(define types-tower
  '(integer rational real complex))

(define (memq item x)
  (cond 
    ((null? x) #f)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))

#|
(define (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    ; select which highest level type of all is.
    (define (find-highest-type types-list)
      (define (iter lowest-type types-list)
        (if (null? types-list)
            lowest-type
            (if (eq? 'LT (compare-type lowest-type (car types-list)))
                (iter (car types-list) (cdr types-list))
                (iter lowest-type (cdr types-list)))))
      (iter 'integer types-list))
    ; coerced lower type to the type.
    (define (raise->type x type)
      (if (eq? (type-tag x) type)
          x
          (raise->type (raise x) type)))
    ; compare types
    (define (compare-type x y)
      (define (iter tower x y)
        (if (null? tower)
            #f
            (let 
              ((car-tower (car tower)))
              (if (eq? car-tower x)
                  (if (eq? car-tower y)
                      'EQ
                      (and (memq y (cdr tower)) 'LT))
                  (if (eq? car-tower y)
                      (and (memq x (cdr tower)) 'GT)
                      (iter (cdr tower) x y))))))
      (or (iter types-tower x y)
          (error "Bad types -- COMPARE-TYPE" (list x y))))
    (define maybe-drop
      (if (memq op '(equ? =zero? raise project))
          (lambda (x) x)
          drop))
    ; inner procedure
    (let
      ((proc (get op type-tags)))
      (if proc
          (maybe-drop (apply proc (map contents args)))
          (let
            ((highest (find-highest-type type-tags)))
            (let
              ((args-bis (map (lambda (a) (raise->type a highest)) args)))
              (let
                ((type-tags-bis (map type-tag args-bis)))
                (let
                  ((proc-bis (get op type-tags-bis)))
                  (if proc-bis
                      (maybe-drop (apply proc-bis (map contents args-bis)))
                      (error "No method for these types -- APPLY-GENERIC"
                             (list op type-tags)))))))))))

(define (drop x)
  (cond
    ((boolean? x) x)
    ((eq? (type-tag x) 'integer) x)
    (else
      (let
        ((dropped (project x)))
        (drop dropped)
        x))))
|#
(define (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    (define (do-coerce x type)
      (let ((coercion (get-coercion (type-tag x) type)))
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
          (let 
            ((proc-coerceds (find-proc-coerceds op args type-tags)))
            (if proc-coerceds
                (apply (car proc-coerceds) (map contents (cdr proc-coerceds)))
                (error "No method for these types -- APPLY-GENERIC"
                       (list op type-tags))))))))

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

; type tags
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; table for coercion
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

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
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'sine '(scheme-number)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'arctangent '(scheme-number)
       (lambda (x) (tag (atan x))))
  (put 'squareroot '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'square '(scheme-number)
       (lambda (x) (tag (* x x))))
  (put 'make '(scheme-number)
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; integer procedure -----------------------------------------------

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (define (integer->rational n)
    (make-rational n 1))  
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (zero? x)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise '(integer)
       (lambda (x) (integer->rational x)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

; rational procedure ----------------------------------------------

(define factor-rational 100000000)  
  
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
  (define (cosine x)
    (make-rat
     (inexact->exact 
       (truncate (* factor-rational (cos (/ (numer x) (denom x))))))
     factor-rational))
  (define (sine x)
    (make-rat
     (inexact->exact 
       (truncate (* factor-rational (sin (/ (numer x) (denom x))))))
     factor-rational))
  (define (arctangent x)
    (make-rat
     (inexact->exact (truncate (* factor-rational (atan (/ (numer x) (denom x))))))
     factor-rational))
  (define (squareroot x)
    (make-rat
     (inexact->exact (truncate (* factor-rational (sqrt (/ (numer x) (denom x))))))
     factor-rational))
  (define (square x)
    (make-rat (* (numer x) (numer x)) (* (denom x) (denom x))))
  (define (rational->real x)
    (make-real (/ (numer x) (denom x))))
  (define (rational->integer x)
    (make-integer (numer x)))
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
  (put 'cosine '(rational) cosine)
  (put 'sine '(rational) sine)
  (put 'arctangent '(rational) arctangent)
  (put 'squareroot '(rational) squareroot)
  (put 'square '(rational) square)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational)
       (lambda (x) (rational->real x)))
  (put 'project '(rational) rational->integer)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

; real procedure ----------------------------------------------

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  (define (real->rational x)
    (make-rational (inexact->exact (truncate x)) 1))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (zero? x)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real)
       (lambda (x) (real->complex x)))
  (put 'project '(real) real->rational)
  'done)

(define (make-real n)
  ((get 'make 'real) n))

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
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular)
       (lambda (z) (make-real (real-part z))))
  (put 'imag-part '(rectangular)
       (lambda (z) (make-real (imag-part z))))
  (put 'magnitude '(rectangular)
       (lambda (z) (make-real (magnitude z))))
  (put 'angle '(rectangular)
       (lambda (z) (make-real (angle z))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

; polar procedure ------------------------------------------------

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar)
       (lambda (z) (make-real (real-part z))))
  (put 'imag-part '(polar)
       (lambda (z) (make-real (imag-part z))))
  (put 'magnitude '(polar)
       (lambda (z) (make-real (magnitude z))))
  (put 'angle '(polar)
       (lambda (z) (make-real (angle z))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; complex procedure ---------------------------------------------

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (contents (real-part z1)) (contents (real-part z2)))
     (+ (contents (imag-part z1)) (contents (imag-part z2)))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (contents (real-part z1)) (contents (real-part z2)))
     (- (contents (imag-part z1)) (contents (imag-part z2)))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (contents (magnitude z1)) (contents (magnitude z2)))
     (+ (contents (angle z1))     (contents (angle z2)))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (contents (magnitude z1)) (contents (magnitude z2)))
     (- (contents (angle z1))     (contents (angle z2)))))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define (equ? x y)
    (and (= (contents (real-part x)) (contents (real-part y)))
         (= (contents (imag-part x)) (contents (imag-part y)))))
  (define (=zero? x)
    (and (zero? (contents (real-part x))) 
         (zero? (contents (imag-part x)))))
  (define (complex->real z) (real-part z))
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
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (eq? z1 z2)))
  (put '=zero? 'complex
       (lambda (z1) (=zero? z1)))
  (put 'project '(complex) complex->real)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package) 
(install-rational-package)
(install-complex-package)
(install-integer-package)
(install-real-package)

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
(define (raise x)     (apply-generic 'raise x))  
(define (project x)   (apply-generic 'project x)) 
(define (sine x)       (apply-generic 'sine x))
(define (cosine x)     (apply-generic 'cosine x))
(define (arctangent x) (apply-generic 'arctangent x))
(define (squareroot x) (apply-generic 'squareroot x))
(define (square x)     (apply-generic 'square x))

; rectangular procedure ------------------------------------------

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)  (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
                term-list
                (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero-term? L)
    (or (empty-termlist? L)
        (and (=zero? (coeff (first-term L)))
             (=zero-term? (rest-terms L)))))
  (define (=zero-poly? p) (=zero-term? (term-list p)))
  ;; add
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly
          (variable p1)
          (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms l1 l2)
    (cond
      ((empty-termlist? l1) l2)
      ((empty-termlist? l2) l1)
      (else
        (let
          ((t1 (first-term l1))
           (t2 (first-term l2)))
          (cond
            ((< (order t2) (order t1))
             (adjoin-term
               t1 (add-terms (rest-terms l1) l2)))
            ((< (order t1) (order t2))
             (adjoin-term
               t2 (add-terms l1 (rest-terms l2))))
            (else
              (adjoin-term
                (make-term (order t1) (add (coeff t1) (coeff t2)))
                (add-terms (rest-terms l1) (rest-terms l2)))))))))
  ;; mul
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly
          (variable p1)
          (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let
          ((t2 (first-term l)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms l))))))
  ;; interface to rest of the system
  (define (tag p) 
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
  
; test ----------------------------------------------------------- 
(install-polynomial-package)

(define p1 (make-polynomial 'y '((1 2) (0 1))))
(define p2 (make-polynomial 'y '((1 -2) (0 -1))))
(print (add p1 p2)) ; (polynomial y)
 
; END