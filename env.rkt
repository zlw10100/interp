#lang racket

(require "struct.rkt")

;; hash table

(define (insert-hash-table! key value table)
  (hash-set! table key value))

(define (lookup-hash-table key table)
  (hash-ref table key (lookup-not-found)))

(define (make-hash/kv keys values)
  (define (make ks vs ht)
    (if (empty? ks)
        ht
        (begin
          (insert-hash-table! (car ks) (car vs) ht)
          (make (cdr ks) (cdr vs) ht))))
  (make keys values (make-hash)))
    
;; env(mutable)
(define (make-empty-env)
  (list (make-hash)))

(define (make-env keys values)
  (list (make-hash/kv keys values)))

(define (make-env/items items)
  (list (make-hash/kv
         (map car items)
         (map cdr items))))

(define (extend-env new-env env)
  (append new-env env))

(define (lookup-env key env)
  (if (empty? env)
      (lookup-not-found)
      (let ([value (lookup-hash-table key (car env))])
        (if (lookup-not-found? value)
            (lookup-env key (cdr env))
            value))))

(define (update-current-env! key value env)
  (if (empty? env)
      (error "env is empty")
      (insert-hash-table! key value (car env))))

(define (update-first-defined-env! key value env)
  (if (empty? env)
      (error "cannot set variable before its definition, variable:" key)
      (if (lookup-not-found? (lookup-hash-table key (car env)))
          (update-first-defined-env! key value (cdr env))
          (insert-hash-table! key value (car env)))))

(provide lookup-not-found?)
(provide make-empty-env)
(provide make-env)
(provide make-env/items)
(provide extend-env)
(provide lookup-env)
(provide update-current-env!)
(provide update-first-defined-env!)
