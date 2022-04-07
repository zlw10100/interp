#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../env.rkt")

(define (set-key exp)
  (cadr exp))

(define (set-value-exp exp)
  (caddr exp))

(define (interp-cps/set! exp env k)
  (interp-cps
   (set-value-exp exp)
   env
   (lambda (set-value)
     (k (update-first-defined-env!
         (set-key exp)
         set-value
         env)))))

(provide interp-cps/set!)
