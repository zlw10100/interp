#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../struct.rkt")
(require "../env.rkt")
(require "../util.rkt")

(define (quote-exp exp)
  (cadr exp))

(define (interp-cps/quote exp env k)
  (k (quote-exp exp)))

(provide interp-cps/quote)
