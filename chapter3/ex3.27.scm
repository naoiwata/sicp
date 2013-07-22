;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.27.
;;

; ------------------------------------------------------------------------
; question
; ------------------------------------------------------------------------

; former solution
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
      (+ (fib (- n 1))
         (fib (- n 2))))))

; solution by using memo
(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (insert! key value table)
  (let
    ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

(define (memorize f)
  (let
    ((table (make-table)))
    (lambda (x)
      (let
        ((priviously-coumputed-result (lookup x table)))
        (or priviously-coumputed-result
            (let
              ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memorize 
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (memo-fib (- n 1))
             (memo-fib (- n 2))))))))

; ------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------

(print (memo-fib 10)) ; 55
(print (memo-fib 100)) ; 354224848179261915075
(print (memo-fib 1000)) ; 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875

; ------------------------------------------------------------------------
; discussion
; ------------------------------------------------------------------------

; memo-fib -> (memorize fib)
#|
memo-fib手続きを(memorize fib)にすると, この手続きのfibは毎回fib手続きを呼び出し評価する為, 手続きのステップ数はθ(n^n)となる. 
一方memo-fibを使う場合は, 一度評価されたfib手続きはtableに保存される(キャッシュとして残る)ので, 一度評価したfib手続きかどうかlookup手続きによって値を探索できるため, ステップ数はθ(n)となる. 
|#

; END
