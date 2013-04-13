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
              ((coerceds (map (lambda (x) (do-coerce x type))) args))
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

; END