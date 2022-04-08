#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../struct.rkt")
(require "../env.rkt")
(require "../util.rkt")

(define (callcc-param exp)
  (car (car (cdadr exp))))

(define (callcc-body exp)
  (cddadr exp))

(define (interp-cps/callcc exp env k)
  ;(printf "callcc-param: ~a, callcc-body: ~a~n" (callcc-param exp) (callcc-body exp))
  (let ([new-env (extend-env
                  (make-env/items `((,(callcc-param exp) . ,(continuation k))))
                  env)])
    (map-cps
     (lambda (e k) (interp-cps e new-env k))
     (callcc-body exp)
     (lambda (body-values)
       (k (last body-values))))))

(provide interp-cps/callcc)
