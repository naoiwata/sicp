;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.63.
;;

; ------------------------------------------------------------------------
; solution
; ------------------------------------------------------------------------

(add-load-path "./pages/" :relative)
(load "stream.scm")

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                   (lambda (guess)
                     (sqrt-improve guess x))
                   guesses)))
  guesses)

(define average
  (lambda (a b)
    (/ (+ a b) 2)))

; Louis
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map 
                 (lambda (guess) 
                   (sqrt-improve guess x))
                 (sqrt-stream x))))

; Louis の方法では各々の項を導出する際に各々の項ごとに毎回sqrt-streamを呼び出し第1項から順に目的のn項までを
; 演算する必要がある. 一方正規の方法では, guesses手続きによって第1項からn項までの数列をあらかじめ作ってメモ化
; しておけるので, sqrt-streamを呼び出した時は目的のn項のみを返す. つまり, 計算のステップ数が本来ならばθ(n)
; だったのがLouisの方法ではθ(n^2)に増大する. また, 正規の方法のステップ数が少ないのはメモ化によるものなので, 
; この手法からメモ化を使用しないならば正規の方法もLouisの方法もθ(n^2)となり効率に差はない.

; END