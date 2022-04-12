#lang racket


;; reset-ks

(define reset-ks '())

(define (enqueue-reset-ks! rk)
  (set! reset-ks
        (append (list rk)
                reset-ks)))

(define (dequeue-reset-ks!)
  (let ([rk (car reset-ks)])
    (set! reset-ks (cdr reset-ks))
    rk))

(define (clear-reset-ks!)
  (set! reset-ks '()))

(define (reset-ks-empty?)
  (empty? reset-ks))

;; shift call flag

(define shift-call #f)

(define (set-shift-call!)
  (set! shift-call #t))

(define (unset-shift-call!)
  (set! shift-call #f))

(define (check-shift-call!)
  (if shift-call
      (begin
        (unset-shift-call!)
        #t)
      #f))

(provide enqueue-reset-ks!)
(provide dequeue-reset-ks!)
(provide clear-reset-ks!)
(provide reset-ks-empty?)
(provide set-shift-call!)
(provide check-shift-call!)
(provide shift-call)
