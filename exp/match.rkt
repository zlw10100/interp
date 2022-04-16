#lang racket

(require racket/lazy-require)

(lazy-require ["../interp.rkt" (interp-cps self-valued?)])

(require "../struct.rkt")
(require "../util.rkt")
(require "../env.rkt")

(define (match-exp exp)
  (cadr exp))

(define (match-items exp)
  (cddr exp))

(define (item-pattern item)
  (car item))

(define (item-body item)
  (cdr item))

(define (interp-cps/match exp env k)
 ; (printf "match exp: ~a~n" exp)
  (interp-cps
   (match-exp exp) env (lambda (v)
                         (k (do-multi-matchs v (match-items exp) env)))))

(define (do-multi-matchs v items env)
  (if (empty? items)
      (error "no pattern was matched for: " v)
      (let ([result (do-match v (car items) env)])
        (if (not-matched? result)
            (do-multi-matchs v (cdr items) env)
            result))))

(define (do-match v item env)
  (let* ([pattern (item-pattern item)]
         [body (item-body item)]
         [matched-env (check-match v pattern)])
    (if (not-matched? matched-env)
        (not-matched)
        (let ([new-env (extend-env
                        (list matched-env)
                        env)])         
          (map-cps
           (lambda (e k) (interp-cps e new-env k))
           body
           (lambda (body-values)
             (last body-values)))))))

(define (check-match v pattern)
  (let* ([matched-env (make-hash)]
         [matched (check-match1 v pattern matched-env)])
    (if matched
        (begin
         ; (printf "matched pattern: ~a~n" pattern)
          ;(printf "matched-env: ~a~n" matched-env)
          matched-env)
        (begin
          ;(printf "not matched: ~a~n~n" pattern)
          (not-matched)))))

(define (check-match1 v pattern matched-env)
  ;(printf "check-match1, v: ~a, pattern: ~a, matched-env: ~a~n" v pattern matched-env)
  (match pattern

    [(? self-valued? p)
     (equal? v p)
     ]
    
    [(? symbol? p)
     (let ([bind-value (get-bind-value matched-env p)])
       (cond
         [(not-bind? bind-value) (begin
                                   (bind! matched-env p v)
                                   #t)]
         [(equal? bind-value v) #t]
         [else #f]))
     ]
   
    [`(quote ,e)
     (equal? e v)]

    [`(quasiquote ,e)
     (check-match1 v e matched-env)
     ]

    [`(unquote-splicing ,e)
     (if (list? e)
         (error "unquote-splicing LIST in match exp is not supported: " e)
         (let ([matched (check-match1 v e matched-env)])
           (if matched
               (bind!
                matched-env
                e
                (list (get-bind-value matched-env e)))
               (not-matched))))
     ]

    [(list 'unquote e)
     (if (list? e)
         (error "unquote LIST in match exp is not supported: " e)
         (begin
           ;(printf "match unquote, e: ~a~n" e)
           (check-match1 v e matched-env)))
     ]

    [`(list)
     (equal? v '())
     ]

    [`(list ,e1 ,e2 ...)
     (check-match1 v `(,e1 ,@e2) matched-env)
     ]

    [`(,e1 ,e2 ...)
     (cond
       [(not (list? v)) #f]
       [(empty? v) #f]
       [else (and
              (check-match1 (car v) e1 matched-env)
              (check-match1 (cdr v) e2 matched-env))])
     ]
    ))
         
(define (get-bind-value matched-env k)
  (hash-ref matched-env k (not-bind)))

(define (bind! matched-env k v)
  (hash-set! matched-env k v))

(provide interp-cps/match)
