#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")
(require "../env.rkt")
              
(provide
 interp-cps/for
 interp-cps/for/list
 interp-cps/for*
 interp-cps/for*/list
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
;6. sequence的辅助函数stop-before、after-before、in-string、in-list、in-vector、in-hash
;7. 提供break

(define (interp-cps/for exp env k)
  (let ([headers (for-headers exp)])
    (if (empty? headers)
        (map-cps
         (lambda (e k) (interp-cps e env k))
         (for-body exp)
         (lambda (body-values)
           (k (void))))
        (let* ([refs (map car headers)]
               [seqs (map-cps
                      (lambda (e k) (interp-cps e env k))
                      (map cadr headers)
                      (lambda (x) x))])
          (k (iter seqs refs (for-body exp) env (void) (lambda (body-values rest-values)
                                                         rest-values)))))))

(define (interp-cps/for/list exp env k)
  (let ([headers (for-headers exp)])
    (if (empty? headers)
        (map-cps
         (lambda (e k) (interp-cps e env k))
         (for-body exp)
         (lambda (body-values)
           (k (list (last body-values)))))
        (let* ([refs (map car headers)]
               [seqs (map-cps
                      (lambda (e k) (interp-cps e env k))
                      (map cadr headers)
                      (lambda (x) x))])
          (k (iter seqs refs (for-body exp) env '() (lambda (body-values rest-values)
                                                      (cons (last body-values) rest-values))))))))

(define (interp-cps/for* exp env k)
  (let* ([headers (for-headers exp)]
         [first-header (car headers)]
         [rest-headers (cdr headers)]
         [body (for-body exp)]
         [nested-for-exp  (make-nested-for-exp headers body)])
    ;(printf "nested-for-exp: ~a~n" nested-for-exp)
    (interp-cps nested-for-exp env k)))
    
(define (interp-cps/for*/list exp env k)
  (let* ([headers (for-headers exp)]
         [first-header (car headers)]
         [rest-headers (cdr headers)]
         [refs (map car headers)]
         [body (for-body exp)]
         [all-values (dke (map-cps
                           (lambda (e k) (interp-cps e env k))
                           (map cadr headers)
                           (lambda (x) x)))])
   ; (printf "refs: ~a~n" refs)
    ;(printf "all-values: ~a~n" all-values)
    (k (loop all-values refs body env))))

(define (loop seqs refs body env)
  (if (empty? seqs)
      '()
      (let* ([values (car seqs)]
             [rest-values (cdr seqs)]
             [new-env (extend-env
                       (make-env refs values)
                       env)])
        (map-cps
         (lambda (e k) (interp-cps e new-env k))
         body
         (lambda (body-values)
           (cons (last body-values)
                 (loop rest-values refs body env)))))))
  
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
