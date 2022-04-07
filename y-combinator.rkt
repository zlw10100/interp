#lang racket

(define Y-exp
  '(lambda (f)
     ((lambda (x) (f (lambda (v) ((x x) v))))
      (lambda (x) (f (lambda (v) ((x x) v)))))))

(provide Y-exp)
