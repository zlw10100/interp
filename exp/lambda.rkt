#lang racket

(require "../struct.rkt")

(define (lambda-params exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (interp-cps/lambda exp env k)
  (k (closure
      (lambda-params exp)
      (lambda-body exp)
      env)))


(provide interp-cps/lambda)
