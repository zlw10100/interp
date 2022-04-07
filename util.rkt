#lang racket

(define (exp-tag exp)
  (car exp))

(define (map-cps f ls k)
  (if (empty? ls)
      (k '())
      (f (car ls)
         (lambda (first-result)
           (map-cps f (cdr ls)
                    (lambda (rest-results)
                      (k (cons first-result rest-results))))))))

(define (make-lambda-exp params body)
  (append
   `(lambda ,params)
   body))

(define (make-call-exp lambda-exp arg-exps)
  (append
   `(,lambda-exp)
   arg-exps))

(define (make-let-exp pairs body)
  (append
   `(let ,pairs)
   body))

(provide exp-tag)
(provide map-cps)
(provide make-lambda-exp)
(provide make-call-exp)
(provide make-let-exp)
