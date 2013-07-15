;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.34.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

(define (squarer a b)
  (multiplier a a b))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(add-load-path "." :relative)
(load "pages/3.3.5.scm")

(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "A" A)
(probe "Square" B)

(set-value! A 10 'user)
; Probe: A = 10
; Probe: Square = 100

(forget-value! A 'user)
; Probe: A = ?
; Probe: Square = ?

(set-value! A 20 'user)
; Probe: A = 20
; Probe: Square = 400

(forget-value! A 'user)
; Probe: A = ?
; Probe: Square = ?

(set-value! B 50 'user)
; Probe: Square = 50

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; (multiplier a b product) は各々の引数のうち, 2つの引数の値が設定されると残りの1つの引数の値を計算して値を設定する手続きになっている. 
;その為, (multiplier a a b) の2つの引数aとaは等価なので, bの値を設定したとき, 引数は1つしか設定されていない状態になり, aの値を設定することができない.

; END