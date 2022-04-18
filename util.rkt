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

(define (make-if-exp pred-exp true-branch-exp false-branch-exp)
  `(if ,pred-exp ,true-branch-exp ,false-branch-exp))

(define (make-cond-exp/items items)
  (append `(cond) items))

(define (make-begin-exp sequence)
  (append `(begin) sequence))

(define (get-duplicate ls not-duplicated)
  (define (check ls s)
    (if (empty? ls)
        not-duplicated
        (let ([e (car ls)]
              [re (cdr ls)])
          (if (set-member? s e)
              e
              (begin
                (set-add! s e)
                (check re s))))))
  (check ls (mutable-set)))

(provide exp-tag)
(provide map-cps)
(provide make-lambda-exp)
(provide make-call-exp)
(provide make-let-exp)
(provide make-if-exp)
(provide make-cond-exp/items)
(provide make-begin-exp)
(provide get-duplicate)
