#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")

(define (cond-items exp)
  (cdr exp))

(define (interp-cps/cond exp env k)
  (let ([items (cond-items exp)])
    (if (empty? items)
        (k (void))      
        (let* ([item (car items)]
               [pred-exp (car item)]
               [true-branch-exp (make-begin-exp (cdr item))])
          (if (eq? pred-exp 'else)
              (interp-cps true-branch-exp env k)
              (let* ([rest-items (cdr items)]
                     [sugar-if-exp (make-if-exp
                                    pred-exp
                                    true-branch-exp
                                    (make-cond-exp/items rest-items))])
                ;(printf "sugar if exp: ~a~n" sugar-if-exp)
                (interp-cps sugar-if-exp env k)))))))

(provide interp-cps/cond)
