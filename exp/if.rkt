#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(define (if-pred-exp exp)
  (cadr exp))

(define (if-true-exp exp)
  (caddr exp))

(define (if-false-exp exp)
  (cadddr exp))

(define (interp-cps/if exp env k)
  (interp-cps
   (if-pred-exp exp)
   env
   (lambda (pred-value)
     (if pred-value
         (interp-cps
          (if-true-exp exp)
          env
          k)
         (interp-cps
          (if-false-exp exp)
          env
          k)))))

(provide interp-cps/if)
