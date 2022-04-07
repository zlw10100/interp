#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")

(define (let*-pairs exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(define (make-nested-let-exp pairs body)
  (if (empty? (cdr pairs))
      (make-let-exp (list (car pairs)) body)
      (make-let-exp
       (list (car pairs))
       (list (make-nested-let-exp (cdr pairs) body)))))

(define (interp-cps/let* exp env k)
  (let ([sugar-let-exp (make-nested-let-exp
                        (let*-pairs exp)
                        (let*-body exp))])
    ;(printf "nested-let-exp: ~a~n"   sugar-let-exp)
    (interp-cps
     sugar-let-exp
     env
     k)))

(provide interp-cps/let*)
