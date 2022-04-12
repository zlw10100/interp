#lang racket

(require "util.rkt")

(require "exp/lambda.rkt")
(require "exp/if.rkt")
(require "exp/let.rkt")
(require "exp/letstar.rkt")
(require "exp/define.rkt")
(require "exp/set.rkt")
(require "exp/begin.rkt")
(require "exp/callcc.rkt")
(require "exp/cond.rkt")

(define (special-form? exp)
  (assq/key-in
   (exp-tag exp)
   special-form-mapping))

(define (assq/key-in key items)
  (not
   (eq? #f
        (assq key items))))

(define (select-special-form-interp-cps exp)
  (cdr
   (assq
    (exp-tag exp)
    special-form-mapping)))

(define special-form-mapping
  `((lambda . ,interp-cps/lambda)
    (if . ,interp-cps/if)
    (let . ,interp-cps/let)
    (let* . ,interp-cps/let*)
    (define . ,interp-cps/define)
    (set! . ,interp-cps/set!)
    (begin . ,interp-cps/begin)
    (call/cc . ,interp-cps/callcc)
    (cond . ,interp-cps/cond)
    ))

(provide special-form?)
(provide select-special-form-interp-cps)
