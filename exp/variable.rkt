#lang racket

(require "../env.rkt")

(define (variable? exp)
  (symbol? exp))

(define (variable-name exp)
  exp)

(define (interp-cps/variable exp env k)
  (let* ([key (variable-name exp)]
         [value (lookup-env key env)])
    (if (lookup-not-found? value)
        (error "not found variable:" key)
        (k value))))

(provide variable?)
(provide interp-cps/variable)
