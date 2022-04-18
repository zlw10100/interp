#lang racket

(require racket/set)
(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])

(require "../util.rkt")
(require "../env.rkt")

(define (letrec-pairs exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (interp-cps/letrec exp env k)
  (let* ([rec-env (make-empty-env)]
         [new-env (extend-env rec-env env)]
         [pairs (letrec-pairs exp)]
         [names (map car pairs)])
    
    (check-duplicate-name names)
    
    (for ([pair pairs])
      (let ([name (car pair)]
            [value-exp (cadr pair)])
        (interp-cps
         value-exp
         new-env
         (lambda (value)
           (update-current-env! name value new-env)))))

    (map-cps
     (lambda (e k)
       (interp-cps e new-env k))
     (letrec-body exp)
     (lambda (body-values)
       (k (last body-values))))))

(define (check-duplicate-name names)
  (let* ([not-duplicated (lambda (x) x)]  ; not-duplicated可以是任何临时内存对象
         [result (get-duplicate names not-duplicated)])
    (if (eq? result not-duplicated)
        (void)
        (error "duplicate identifier in: " result))))

(provide interp-cps/letrec)
