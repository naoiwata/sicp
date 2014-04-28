;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.14
;; Draw the tree illustrating the process generated by the count-change procedure of section 1.2.2 in making change for 11 cents. What are the orders of growth of the space and number of steps used by this process as the amount to be changed increases?
;;

(add-load-path "." :relative)
(load "p22-1.scm")

(print (count-change 11))
; 4

; step : Θ(n)
; space: (Θ(n))^(kind-of-coins)
; see http://naoiwata.blogspot.jp/2013/02/sicp-orders-of-growth.html

;; END
