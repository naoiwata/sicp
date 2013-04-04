;; @author naoiwata
;; SICP Chapter2
;; question 2.74

; (a)
#|
 type    | main-company | sub-company
 -------------------------------------
 record  |              |
 address |              |
 salary  |              |
|#

(define company-table (make-hash-table))

(define (put op type item)
  (if
    (hash-table-exists? company-table type)
    (let
      ((type-table (hash-table-get company-table type)))
      (hash-table-put! type-table op item))
    (let
      ((new-type-table (make-hash-table)))
      (hash-table-put! new-type-table op item)
      (hash-table-put! company-table type new-type-table))))

(define (get op type)
  (if
    (hash-table-exists? company-table type)
    (let
      ((type-table (hash-table-get company-table type)))
      (hash-table-get type-table op))
    #f))

; sub-company DateBase : Example
(define sub-company-data
  '((alpaca tokyo 300000) (cat osaka 400000) (penguin sapporo 500000)))

(define (install-sub-package)
  ;; internal procedures
  (define (get-name record) (car record))
  (define (get-address record) (cdr record))
  (define (get-salary record) (cddr record))
  (define (make-record name address salary) (list get-name get-address get-salary))
  (define (make-db records) (cons 'sub records))
  (define (get-record db name)
    (let iter
      ((rest-records (cdr db)))
      (cond
        ((null? rest-records)
         #f)
        ((eq? name (get-name (car rest-records)))
         (car rest-records))
        (else
          (iter (cdr rest-records))))))
  ;; interface
  (put 'get-name 'sub get-name)
  (put 'get-address 'sub get-address)
  (put 'get-salary 'sub get-salary)
  (put 'make-record 'sub make-record)
  (put 'make-db 'sub make-db)
  (put 'get-record 'sub get-record)
  'done)

;;; (a)
(install-sub-package)

(define sub-db 
  ((get 'make-db 'sub) sub-company-data))

(define (get-record db name)
  ((get 'get-record 'sub) db name))

; test
(get-record sub-db 'alpaca) ; (alpaca tokyo 300000)
(get-record sub-db 'cat) ; (cat osaka 400000)
(get-record sub-db 'penguin) ; (penguin sapporo 500000)
(get-record sub-db 'dog) ; #f

;;; (b)
(define (get-salary db name)
  (let
    ((record (get-record db name)))
    (if (eq? record #f)
        #f
        (car ((get 'get-salary 'sub) record)))))

; test
(get-salary sub-db 'alpaca) ; 300000
(get-salary sub-db 'cat) ; 400000
(get-salary sub-db 'penguin) ; 500000
(get-salary sub-db 'dog) ; #f

;;; (c)
(define (find-employee-record all-db name)
  (let iter
    ((db all-db))
    (if (null? db)
        #f
        (let
          ((get-name (get-record (car db) name)))
          (if (eq? get-name #f)
              (iter (cdr db))
              get-name)))))

; another-company DataBase : Example
(define ano-company-data
  '((blue 1000000 usa) (green 2000000 uk) (pink 3000000 nz)))

(define (install-ano-package)
  ;; internal procedures
  (define (get-name record) (car record))
  (define (get-address record) (cddr record))
  (define (get-salary record) (cdr record))
  (define (make-record name address salary) (list get-name get-address get-salary))
  (define (make-db records) (cons 'ano records))
  (define (get-record db name)
    (let iter
      ((rest-records (cdr db)))
      (cond
        ((null? rest-records)
         #f)
        ((eq? name (get-name (car rest-records)))
         (car rest-records))
        (else
          (iter (cdr rest-records))))))
  ;; interface
  (put 'get-name 'ano get-name)
  (put 'get-address 'ano get-address)
  (put 'get-salary 'ano get-salary)
  (put 'make-record 'ano make-record)
  (put 'make-db 'ano make-db)
  (put 'get-record 'ano get-record)
  'done)

(install-ano-package)

(define ano-db 
  ((get 'make-db 'ano) ano-company-data))

(define (get-record-ano db name)
  ((get 'get-record 'ano) db name))

(define all-db (list sub-db ano-db))

(find-employee-record all-db 'cat) ; (cat osaka 400000)
(find-employee-record all-db 'pink) ; (pink 3000000 nz)
(find-employee-record all-db 'dammy) ; #f

;;; (d)
#|
問題：この企業が別の会社を合併した時、新しい従業員情報を中央システムに組み込むにはどういう変更をすべきか。
新たなデータベースに対応した型タグ付きのデータと専用のpackage、そのpackegeから操作できるような手続きを実装すればよい。
|#

; END