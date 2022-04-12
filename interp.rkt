#lang racket

(require "special-form.rkt")
(require "native.rkt")
(require "builtin.rkt")
(require "env.rkt")
(require "y-combinator.rkt")
(require "global.rkt")

(require "exp/variable.rkt")
(require "exp/call.rkt")

;; interp cps

(define (interp-cps exp env k)
  (cond
    [(self-valued? exp) (k exp)]
    [(variable? exp) (interp-cps/variable exp env k)]
    [(special-form? exp) ((select-special-form-interp-cps exp) exp env k)]
    [(call? exp) (interp-cps/call exp env k)]
    [else (error "unsupported expression:" exp)]))

;; self-valued

(define (self-valued? exp)
  (or
   (number? exp)
   (string? exp)
   (boolean? exp)
   (eq? '() exp)))

;; api

(define (interp exp)
  (let ([init-env (extend-env
                   (make-builtin-env)
                   (make-native-env))]
        [final-exp  
         `(reset  ;; 这里帮用户做了初始reset的客户代码
           (begin
             (define Y ,Y-exp)  ;; 这里帮用户做了Y combinator的客户代码
             ,exp
             ))]
        )
    ;(printf "final-exp: ~a~n" final-exp)
    (clear-reset-ks!)
    (interp-cps final-exp init-env (lambda (x) x))))

(provide interp-cps)
(provide interp)
