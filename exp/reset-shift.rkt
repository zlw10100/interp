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
  (begin
    (enqueue-reset-ks! k)
   ; (printf "reset k: ~a ~a~n" k (k 10))
    (map-cps
     (lambda (e k)
       (interp-cps e env (lambda (result)
                      ;     (printf "result: ~a ready to call reset k, shift-call: ~a~n"
                       ;            result
                        ;           shift-call)
                          ; (printf "(k result) is: ~a~n" (k result))
                           (if (check-shift-call!)
                               ;; shift里面对k的调用，执行到这里不再调用后续k，模拟reset和shift之间的continuation
                               result  
                               (k result)))))
     (reset-body exp)
     (lambda (body-values)
       (k (last body-values))))))

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
     (lambda (e k) (interp-cps e new-env k))
     (shift-body exp)
     (lambda (body-values)
       (let ([reset-k (dequeue-reset-ks!)])
         (reset-k (last body-values)))))))

(provide interp-cps/reset)
(provide interp-cps/shift)
