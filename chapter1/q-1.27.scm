;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.27
;;

; how to search carmichael number is shown on ref.

(add-load-path ".")
(load "q-1.25")

; carmichael numbers are 561, 1105, 1729, 2465, 2821, 6601, 8911...
(fast-prime? 561 2)
; -> #t
(fast-prime? 1105 2)
; -> #t
(fast-prime? 1729 2)
; -> #t
(fast-prime? 2465 2)
; -> #t
(fast-prime? 2821 2)
; -> #t

; ref(carmichel numbers) : http://naoiwata.blogspot.jp/2013/02/sicp-126-testing-for-primality.html (fig. 4)
; END