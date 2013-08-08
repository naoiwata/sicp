;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.52.
;;

; ------------------------------------------------------------------------
; solution memorized
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define sum 0)
; sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum: 0

(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))
; seq: ((accum 1) (accum 2) (accum 3) ... (accum 20)) 
; eval: (accum 1) 
; sum: 1, evaluated only (accum 1) as (car seq)

(define y (stream-filter even? seq))
; eval: (accum 2) (accum 3)
; sum: 6, means 1 + 2 + 3

(define z (stream-filter 
            (lambda (x) (= (remainder x 5) 0))
            seq))
; eval: (accum 4)
; sum: 10, means 6 + 4

(print(stream-ref y 7))
; eval: (accum 5) (accum 6) (accum 7) (accum 8) (accum 9) (accum 10) (accum 11) (accum 12) (accum 13) (accum 14) (accum 15) (accum 16) 
; 136, means 10 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16

(display-stream z)
; eval: (accum 17) (accum 18) (accum 19) (accum 20)
; sum: 210, means 136 + 17 + 18 + 19 + 20
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210

(print sum) 
; 210

; seq
;  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
;  1   3   6  10  15  21  28  36  45  55  66  78  91 105 120 136 153 171 190 210
 
; ------------------------------------------------------------------------
; solution not-memorized, not test yet
; ------------------------------------------------------------------------

(define sum 0)
; sum: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum: 0

(define seq
  (stream-map accum (stream-enumerate-interval 1 20)))
; seq: ((accum 1) (accum 2) (accum 3) ... (accum 20)) 
; eval: (accum 1) 
; sum: 1, evaluated only (accum 1) as (car seq)

(define y (stream-filter even? seq))
; eval: (accum 1) (accum 2) (accum 3)
; sum: 6, means 1 + 2 + 3
; seq
;  1   2   3   
;  1   3   6  
 
(define z (stream-filter 
            (lambda (x) (= (remainder x 5) 0))
            seq))
; eval: (accum 2) (accum 3) (accum 4)
; sum: 15, means 
; seq
;  1   2   3   4   
;  1   8  11  15   

(print(stream-ref y 7))
; eval: (accum 5) (accum 6) (accum 7) (accum 8) (accum 9) (accum 10) (accum 11) (accum 12) (accum 13) (accum 14) (accum 15) (accum 16) (accum 17) 
; 162, means 15 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17
; seq
;  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
;  1   8  11  19  24  30  37  45  54  64  75  87 100 113 129 145 162
 
(display-stream z)
; eval: (accum 17) (accum 18) (accum 19) (accum 20)
; sum: 362, means 162 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20
; 15
; 180
; 230
; 305
;  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
;  1   8  11  19 162 173 180 188 197 207 218 230 243 257 272 288 305 323 342 362

(print sum) 
; 362
 
; END