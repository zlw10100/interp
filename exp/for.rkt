#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")
(require "../env.rkt")
              
(provide
 interp-cps/for
 interp-cps/forlist
 )

(define (for-headers exp)
  (cadr exp))

(define (for-body exp)
  (cddr exp))

;TODO
;1. header后面的内容扩展
;2. for和for/list，估计还有其他类似for，有着很多通用逻辑和代码
;3. header里面可以是非list类型
;4. 提供sequence struct类型和序列iter的接口，实现：字符串、数字、列表、哈希表、数组
;5. for*、for*/list
;6. sequence的辅助函数stop-before、after-before、in-string、in-list、in-vector、in-hash
;7. 提供break

;; for

(define (interp-cps/for exp env k)
  (let* ([headers (for-headers exp)]
         [refs (map car headers)]
         [seqs (map-cps
                (lambda (e k) (interp-cps e env k))
                (map cadr headers)
                (lambda (x) x))])
    (k (iter seqs refs (for-body exp) env (void) (lambda (body-values rest-values)
                                                   rest-values)))))

;; for/list

(define (interp-cps/forlist exp env k)
  (let* ([headers (for-headers exp)]
         [refs (map car headers)]
         [seqs (map-cps
                (lambda (e k) (interp-cps e env k))
                (map cadr headers)
                (lambda (x) x))])
    (k (iter seqs refs (for-body exp) env '() (lambda (body-values rest-values)
                                                (cons (last body-values) rest-values))))))
  
(define (iter seqs refs body env unit combine)
  (if (member '() seqs)
      unit
      (let* ([values (map car seqs)]
             [new-env (extend-env
                       (make-env refs values)
                       env)])
        (map-cps
         (lambda (e k) (interp-cps e new-env k))
         body
         (lambda (body-values)
           (combine body-values
                    (iter (map cdr seqs) refs body env unit combine)))))))
