#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")

(define (begin-body exp)
  (cdr exp))

(define (interp-cps/begin exp env k)
  (map-cps
   (lambda (e k) (interp-cps e env k))
   (begin-body exp)
   (lambda (body-values)
     (k (last body-values)))))

(provide interp-cps/begin)
