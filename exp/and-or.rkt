#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")

(define (logic-pred-exps exp)
  (cdr exp))

(define (interp-cps/and exp env k)
  (k (interp-logic
      (logic-pred-exps exp)
      env
      'AND)))

(define (interp-cps/or exp env k)
  (k (interp-logic
      (logic-pred-exps exp)
      env
      'OR)))

; logic: AND/OR
(define (interp-logic pred-exps env logic)
  (cond
    [(empty? pred-exps)
     (if (eq? logic 'AND)
         #t
         #f)]
    [else (let ([pred-exp (car pred-exps)]
                [rest-pred-exps (cdr pred-exps)])
            (interp-cps
             pred-exp
             env
             (lambda (pred-value)
               (check-boolean pred-value)
               (if pred-value
                   (if (eq? logic 'AND)
                       (interp-logic rest-pred-exps env logic)
                       pred-value)
                   (if (eq? logic 'AND)
                       pred-value
                       (interp-logic rest-pred-exps env logic))))))]))

(provide interp-cps/and)
(provide interp-cps/or)
