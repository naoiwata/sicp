;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.81.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (random-stream requests)
  (define (rand req prev)
    (random-update
      (cond
        ((and (symbol? req) (eq? req 'generate)) prev)
        ((and (pair? req) (eq? (car req) 'reset)) (cdr req))
        (else
          (error "Unknown request -- RAND" req)))))
  (define random-numbers
    (cons-stream (rand (stream-car requests) random-init)
                 (stream-map rand (stream-cdr requests) random-numbers)))
  random-numbers)
