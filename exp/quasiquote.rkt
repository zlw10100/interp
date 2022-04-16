#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps)])


(define (quasiquote-exp exp)
  (cadr exp))

(define (interp-cps/quasiquote exp env k)
  ; (printf "quasiquote exp: ~a~n" exp)
  (transform (quasiquote-exp exp) env (lambda (x y)
                                        (k y))))

(define (transform exp env ctx)
  (match exp
    
    [(list 'unquote e)
     (interp-cps e env (lambda (v)
                         (ctx cons v)))
     ]
    
     [(list 'unquote-splicing e)
      (interp-cps e env (lambda (v)
                          (ctx append v)))
      ]

    [`(,e1  ,e2 ...)
     (transform e1 env (lambda (op1 v1)
                         (transform e2 env (lambda (op2 v2)
                                             (if (eq? op1 append)
                                                 (if (list? v1)
                                                     (ctx cons (op1 v1 v2))
                                                     (if (empty? v2)
                                                         (ctx cons v1)
                                                         (error "unquote-splicing need a list, but got: " v1)))
                                                 (ctx cons (op1 v1 v2)))))))
     ]

    [_ (ctx cons exp)]
    ))


(provide interp-cps/quasiquote)
