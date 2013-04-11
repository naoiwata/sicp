;; @author naoiwata
;; SICP Chapter2
;; question 2.81

#|
(a)(b)
互いに異なる型であれば強制型変換を実行する処理ならば、互いが同じ型になった時点でその処理は停止する。しかし、既に同じ型を持っていても互いの型へ強制変換を強制してしまうと、処理が開始されてから互いに同じ型になったとしても再び処理が繰り返され、無限ループに陥ってしまう。
よって、同じ型でも強制型変換を実行する処理をすべきだというLouisの考えはapply-gereric処理が無限ループに陥ってしまうので、この主張は正しくない。
|#

; (c)
(define (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    (let 
      ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let 
                      ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                      (cond
                        (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                        (else
                          (error "No method for these types"
                                 (list op type-tags))))))
                (error "No method for these types"
                     (list op type-tags))))))))

; END