#lang racket

(require racket/lazy-require)

(lazy-require ["y-combinator.rkt" (Y)]
              ["interp.rkt" (interp-cps make-init-env)])

(require "struct.rkt")
(require "env.rkt")
(require "util.rkt")

(define (builtin/not x)
  (check-boolean x)
  (not x))

(define (builtin/make-base-empty-namespace)
  (namespace (make-empty-env)))

(define (builtin/make-base-namespace)
  (namespace (make-init-env)))

(define (builtin/eval exp [ns (builtin/make-base-empty-namespace)])
  (let ([env (namespace-env ns)])
    (interp-cps exp env (lambda (x) x))))

(define (make-builtin-env)
  (make-env/items
   `(
     (not . ,(builtin builtin/not))
     (make-base-empty-namespace . ,(builtin builtin/make-base-empty-namespace))
     (make-base-namespace . ,(builtin builtin/make-base-namespace))
     (eval . ,(builtin builtin/eval))
     )))

(provide make-builtin-env)
