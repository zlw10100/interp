#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../env.rkt")

(define (define-key exp)
  (cadr exp))

(define (define-value-exp exp)
  (caddr exp))

(define (interp-cps/define exp env k)
  (interp-cps
   (define-value-exp exp)
   env
   (lambda (define-value)
     (k (update-current-env!
         (define-key exp)
         define-value
         env)))))

(provide interp-cps/define)
