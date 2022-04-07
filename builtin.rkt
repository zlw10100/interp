#lang racket

(require racket/lazy-require)

(lazy-require ["y-combinator.rkt" (Y)])

(require "struct.rkt")
(require "env.rkt")

(define (make-builtin-env)
  (make-env/items
   `(
     
     )))

(provide make-builtin-env)
