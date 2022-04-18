#lang racket

(require racket/lazy-require)

(lazy-require ["y-combinator.rkt" (Y)])

(require "struct.rkt")
(require "env.rkt")
(require "util.rkt")

(define (builtin/not x)
  (check-boolean x)
  (not x))

(define (make-builtin-env)
  (make-env/items
   `(
     (not . ,(builtin builtin/not))
     
     )))

(provide make-builtin-env)
