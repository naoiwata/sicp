;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.75.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (sign-change-detector current-value last-value)
  (cond
    ((and (> current-value 0) (< last-value 0)) 1)
    ((and (< current-value 0) (> last-value 0)) -1)
    (else 0)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream sense-data 0)))

(define (make-zero-crossings input-stream last-value last-average)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
