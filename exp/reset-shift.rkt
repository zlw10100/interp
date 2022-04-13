#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../struct.rkt")
(require "../env.rkt")
(require "../util.rkt")
(require "../global.rkt")

;; reset

(define (reset-body exp)
  (cdr exp))

(define (interp-cps/reset exp env k)
  (map-cps
   (lambda (e k)
     (k (interp-cps e env ID)))  ;; 用ID做continuation界定符
   (reset-body exp)
   (lambda (body-values)
     (k (last body-values)))))


;; shift

(define (shift-name exp)
  (cadr exp))

(define (shift-body exp)
  (cddr exp))

(define (interp-cps/shift exp env k)
  (let ([new-env (extend-env
                  (make-env/items `((,(shift-name exp) . ,(pcontinuation k))))
                  env)])
    (map-cps
     (lambda (e k)
       (interp-cps e new-env k))
     (shift-body exp)
     (lambda (body-values)
       (last body-values)))))  ;; 不调用k可以跳过reset...shift之间的continuation

(provide interp-cps/reset)
(provide interp-cps/shift)
