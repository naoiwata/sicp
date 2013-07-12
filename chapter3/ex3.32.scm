;;
;; @author naoiwata
;; SICP Chapter3
;; Exercise 3.32.
;;

; ------------------------------------------------------------------------
; queue
; ------------------------------------------------------------------------

; front
(define (front-ptr queue) (car queue))

; rear
(define (rear-ptr queue) (cdr queue))

; set! front
(define (set-front-ptr! queue item) (set-car! queue item))

; set! rear
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; null? front
(define (empty-queue? queue) (null? (front-ptr queue)))

; make queue
(define (make-queue) (cons '() '()))

; return front
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; insert queue to the rear
(define (insert-queue! queue item)
  (let 
    ((new-pair (cons item '())))
    (cond 
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue)
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))

; delete front
(define (delete-queue! queue)
  (cond 
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue))
    (else
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

; ------------------------------------------------------------------------
; import 3.3.4
; ------------------------------------------------------------------------

; several gates
(define (inverter input output)
  (define (invert-input)
    (let 
      ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else 
          (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let 
      ((new-value
         (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s c)
  (cond ((and (= s 1) (= c 1)) 1)
        ((or
           (and (= s 0) (= c 0))
           (and (= s 1) (= c 0))
           (and (= s 0) (= c 1))) 0)
        (else 
          (error "Invalid signal" s))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (b (make-wire)))
      (inverter a1 not-a1)
      (inverter a2 not-a2)
      (and-gate not-a1 not-a2 b)
      (inverter b output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; half-addr
(define (half-adder a b s c)
  (let 
    ((d (make-wire)) 
     (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; Representing wires
(define (make-wire)
  (let 
    ((signal-value 0) 
     (action-procedures '()))
  (define (set-my-signal! new-value)
    (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
          (call-each action-procedures))
        'done))
  (define (accept-action-procedure! proc)
    (set! action-procedures (cons proc action-procedures))
    (proc))
  (define (dispatch m)
    (cond ((eq? m 'get-signal) signal-value)
          ((eq? m 'set-signal!) set-my-signal!)
          ((eq? m 'add-action!) accept-action-procedure!)
          (else 
            (error "Unknown operation" m))))
  dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; Implementing the agenda
(define (after-delay delay action)
  (add-to-agenda! 
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let 
        ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

; A sample simulation
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) 
  (car s))

(define (segment-queue s) 
  (cdr s))

(define (make-agenda) 
  (list 0))

(define (current-time agenda) 
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) 
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) 
  (car (segments agenda)))

(define (rest-segments agenda) 
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let 
      ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let 
          ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
              (add-to-segments! rest)))))
  (let 
    ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let 
    ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let 
        ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; ------------------------------------------------------------------------
; test-1 : first in, first out
; ------------------------------------------------------------------------

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define and-in-1 (make-wire))
(define and-in-2 (make-wire))
(define and-out  (make-wire))

(probe 'and-out and-out) 
; and-out 0 New-value = 0 
(print (and-gate and-in-1 and-in-2 and-out)) 
; ok
(print (set-signal! and-in-1 0)) 
; done
(print (set-signal! and-in-2 1)) 
; done
(print the-agenda)
; (0 (3 (#<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)>) #<closure (and-gate and-action-procedure #f)>))
(print (propagate)) 
; done
(print (set-signal! and-in-1 1)) 
; done
(print (set-signal! and-in-2 0)) 
; done
(print the-agenda) 
; (3 (6 (#<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)>) #<closure (and-gate and-action-procedure #f)>))
(print (propagate)) 
  ; and-out 6 New-value = 1
  ; and-out 6 New-value = 0
; done

; ------------------------------------------------------------------------
; test-2 : last in, first out
; ------------------------------------------------------------------------

; front
(define (front-ptr queue) queue)

; null? front
(define (empty-queue? queue) (null? (car queue)))

; make queue
(define (make-queue) '(()))

; return front
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (car queue))))

; insert queue to the rear
(define (insert-queue! queue item)
  (let 
    ((new-pair (cons item (car queue))))
    (set-car! queue new-pair)))

; delete front
(define (delete-queue! queue)
  (cond 
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue))
    (else 
      (set-car! queue (cdr (car queue)))
      queue)))

(define (print-queue queue)
  (car queue))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define and-in-1 (make-wire))
(define and-in-2 (make-wire))
(define and-out  (make-wire))

(probe 'and-out and-out) 
; and-out 0 New-value = 0 
(print (and-gate and-in-1 and-in-2 and-out)) 
; ok
(print (set-signal! and-in-1 0)) 
; done
(print (set-signal! and-in-2 1)) 
; done
(print the-agenda)
; (0 (3 (#<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)>) #<closure (and-gate and-action-procedure #f)>))
(print (propagate)) 
; done
(print (set-signal! and-in-1 1)) 
; done
(print (set-signal! and-in-2 0)) 
; done
(print the-agenda) 
; (3 (6 (#<closure (and-gate and-action-procedure #f)> #<closure (and-gate and-action-procedure #f)>) #<closure (and-gate and-action-procedure #f)>))
(print (propagate)) 
; and-out 6 New-value = 1
; done

; ------------------------------------------------------------------------
; Discussion
; ------------------------------------------------------------------------

#|
test-1(最初に入ったものが最初に出る)とtest-2(最後に入ったものが最初に出る)を比較する.
想定通りの正しい動作は前者のtest-1である.
まず, 各々にa, b, cを順に入れる時のデータ構造を考える.
test-1では(a b c), test-2では(c b a)となり双方共にcarである左の値から取り出される.

本問題をテストし出力結果を見ると
test-1は and-out 6 New-value = 0
test-2 は and-out 6 New-value = 1
が出力されているのが分かる.
ここで, 双方が時に従ってどのような状態遷移を経るか考える.

 time |  in-1  | in-2  |  out(new-value)
  0   |   0    |   0   |   -
 -> in-1 = 0, in-2 = 1 を代入
  3   |   0    |   0   |   0   <- (set-signal! and-in-1 0) :先頭の in-1 が代入され new-value が0に定義される
  3   |   0    |   1   |   0   <- (set-signal! and-in-2 1) :後ろの in-2 が代入され in-2 が変化, new-value が0に定義される
 -> in-1 = 1, in-2 = 0 を代入
  6   |   1    |   1   |   1   <- (set-signal! and-in-1 1) :先頭の in-1 が代入され in-1 が変化, new-value が1に定義される(new-1)
  6   |   1    |   0   |   0   <- (set-signal! and-in-2 0) :後ろの in-2 が代入され in-2 が変化, new-value が0に定義される(new-2)

test-1 では出力結果(set-signal!の返り値)を順に登録して行くと ((new-1) (new-2)) の構造になり, 先頭から順に価を返すので解は (new-2)の0を返すが, 
test-2 では出力結果(set-signal!の返り値)を順に登録して行くと ((new-2) (new-1)) の構造になり先頭から順に価を返すので解は (new-1)の1を返し, 
以上のような出力結果になったと考えられる.
よって, 事象駆動シミュレーションではデータ構造の値の代入の順番と代入するデータ構造の場所, そしてそれらの評価の順を考慮して手続きの時間の流れを考える必要があると言える.
|#

; END