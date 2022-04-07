#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")

(define (let-pairs exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (interp-cps/let exp env k)
  (let* ([let-keys (map car (let-pairs exp))]
         [let-value-exps (map cadr (let-pairs exp))]
         [sugar-lambda-exp (make-lambda-exp
                            let-keys
                            (let-body exp))]
         [sugar-call-exp (make-call-exp
                      sugar-lambda-exp
                      let-value-exps)])
    ;(printf "sugar-call-exp: ~a~n" sugar-call-exp)
    (interp-cps sugar-call-exp env k)))

(provide interp-cps/let)
