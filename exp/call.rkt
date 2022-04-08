#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")
(require "../struct.rkt")
(require "../env.rkt")

(define (call? exp)
  (list? exp))

(define (call-op exp)
  (car exp))

(define (call-args exp)
  (cdr exp))

(define (interp-cps/call exp env k)
  (interp-cps
   (call-op exp)
   env
   (lambda (op-value)
     (map-cps
      (lambda (arg-exp k) (interp-cps arg-exp env k))
      (call-args exp)
      (lambda (arg-values)
        (cond
          [(closure? op-value) 
           (let ([local-env (extend-env
                             (make-env
                              (closure-params op-value)
                              arg-values)
                             (closure-env op-value))])
             (map-cps
              (lambda (e k) (interp-cps e local-env k))
              (closure-body op-value)
              (lambda (body-values)
                (k (last body-values)))))]
          [(continuation? op-value)
           (apply (continuation-procedure op-value)
            arg-values)]
          [(builtin? op-value)
           (printf "builtin: ~a~n" (builtin-procedure op-value))
           (k (apply
               (builtin-procedure op-value)
               arg-values))]
          [(native? op-value) (k (apply
                                  (native-procedure op-value)
                                  arg-values))]
          [else (error "incorrect op-value:" op-value)]))))))

(provide call?)
(provide interp-cps/call)
