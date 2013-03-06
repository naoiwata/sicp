;;
;; @author naoiwata
;; SICP Chapter1
;; q-1.37
;;

; (a) linear recursive procedure
(define (cont-franc n d k)
	(define (iter i)
		(if (= k i)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (iter (+ i 1))))))
	(iter 1))

; test
(define (test f a b)
	(cond
		((< a b)
			(test f (+ a 1) b)
			(newline)
			(display a)
			(display " : ")
			(display (f a)))
		(else
			#f)))

(print
	(test
		(lambda (k)
			(cont-franc
				(lambda (i) 1.0)
				(lambda (i) 1.0)
			k))
		1
		20))
; 19 : 0.6180339985218034
; 18 : 0.6180339631667064
; 17 : 0.6180340557275542
; 16 : 0.6180338134001252
; 15 : 0.6180344478216819
; 14 : 0.6180327868852459
; 13 : 0.6180371352785146
; 12 : 0.6180257510729613
; 11 : 0.6180555555555556
; 10 : 0.6179775280898876
; 9 : 0.6181818181818182
; 8 : 0.6176470588235294
; 7 : 0.6190476190476191
; 6 : 0.6153846153846154
; 5 : 0.625
; 4 : 0.6000000000000001
; 3 : 0.6666666666666666
; 2 : 0.5
; 1 : 1.0#<undef>
; -> k is need to over 11 

; (b) iteative procedure
(define (cont-franc-i n d k)
	(define (iter i result)
		(if (= k i)
			result
			(iter (+ i 1) (/ (n i) (+ (d i) result)))))
	(iter 1 0))

; END
