;;
;; @author naoiwata
;; SICP Chapter4
;; Exercise 4.03.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(define deriv-table (make-hash-table))

(define (put op type item)
  (hash-table-put! deriv-table type item))

(define (get op type)
  (hash-table-get deriv-table type))
