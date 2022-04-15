#lang racket

(require "interp.rkt")

;; test
(define (test title x expected)
  (if (equal? x expected)
      (printf "[OK] test: ~a~n~n" title)
      (printf "[FAIL] test: ~a expected: ~a got: ~a~n~n" title expected x)))

(define all-test-cases '())

(define (add-test-cases! tc)
  (set! all-test-cases
        (append all-test-cases
                (list tc))))

(define (test-all)
  (for-each
   (lambda (tc) (tc))
   all-test-cases))

;; test cases

; test lambda

(define (test-interp-lambda)
  (test
   "lambda1"
   (interp '((lambda (x y)
               (+ x y)) 1 9))
   10)

  (test
   "lambda2"
   (interp '((((lambda (x)
                 (lambda (y)
                   (lambda (z)
                     (+ x y z)))) 1) 2) 3))
   6)
  )
(add-test-cases! test-interp-lambda)

; test if

(define (test-interp-if)
  (test
   "if1"
   (interp '(if (< 2 3) "yes" "no"))
   "yes")
  
  (test
   "if2"
   (interp '(if (> 2 3) "yes" "no"))
   "no")

  (test
   "if3"
   (interp '(((lambda (x y)
                (if (= x y)
                    (lambda (a b) (* a b))
                    (lambda (a b) (+ a b)))) 10 100) 5 5))
   10)

  (test
   "if4"
   (interp '(((lambda (x y)
                (if (= x y)
                    (lambda (a b) (* a b))
                    (lambda (a b) (+ a b)))) 100 100) 5 5))
   25)
  )
(add-test-cases! test-interp-if)

; test let

(define (test-interp-let)
  (test
   "let1"
   (interp '(let ([x 2]
                  [y 3])
              100
              (+ x y)))
   5)

  (test
   "let2"
   (interp '(let ([x 2])
              (let ([y 3])
                100
                (+ x y))))
   5)
  )
(add-test-cases! test-interp-let)

; test let*

(define (test-interp-let*)
  (test
   "let*1"
   (interp '(let* ([x 2]
                   [y (* x x)])
              100
              (+ x y)))
   6)

  (test
   "let*2"
   (interp '(let ([x 2])
              (let* ([y (+ x 10)] ; 12
                     [z (+ x y)]) ; 14
                (+ x y z)))) ; 28
   28)

  (test
   "let*3"
   (interp '(let* ([a 23]
                   [b a])
              100
              200
              300
              (+ a b)))
   46)
  )
(add-test-cases! test-interp-let*)

; test define

(define (test-interp-define)
  (test
   "define1"
   (interp '((lambda (x)
               (define y 100)
               (+ x y)) 3))
   103)

  (test
   "define2"
   (interp '((lambda (x)
               (define y 100)
               ((lambda (z)
                  (+ x y z)) 200)) 3))
   303)

  (test
   "define3"
   (interp '((lambda (x)
               (define y 100)
               ((lambda (z)
                  (define y 66)
                  (+ x y z)) 200)) 3))
   269)
  
  (test
   "define4"
   (interp '((lambda ()
               (define x 100)
               (lambda ()
                 (define x 66))
               x)))
   100) 
  )
(add-test-cases! test-interp-define)


; test recursion define function

(define (test-interp-recursion-function)
  (test
   "recursion function1"
   (interp '(begin
              (define sum
                (lambda (n)
                  (if (= n 0)
                      0
                      (+ n (sum (- n 1))))))
              (sum 100)))
   5050)

  (test
   "recursion function2"
   (interp '(begin
              (define fact
                (lambda (n)
                  (if (= n 0)
                      1
                      (* n (fact (- n 1))))))
              (fact 10)))
   3628800)

  (test
   "recursion function3"
   (interp '(begin
              (define fib
                (lambda (n)
                  (if (= n 0)
                      0
                      (if (= n 1)
                          1
                          (+
                           (fib (- n 1))
                           (fib (- n 2)))))))
              (fib 10)))
   55)
  )

(add-test-cases! test-interp-recursion-function)

; set!

(define (test-interp-set!)
  (test
   "set!1"
   (interp '((lambda ()
               (define x 100)
               ((lambda ()
                  (set! x 3)
                  0))
               x)))
   3)

  (test
   "set!2"
   (interp '((lambda (x)
               (define result (* x x))
               (set! x 100)
               ((lambda ()
                  x))) 4))
   100)

  (test
   "set!3"
   (interp '((lambda (x)
               (define result (* x x))
               (set! x 100)
               ((lambda ()
                  x))
               result) 4))
   16)

  )
(add-test-cases! test-interp-set!)

; Y combinator

(define (test-interp-y-combinator)
  (test
   "Y-combinator1"
   (interp '((Y (lambda (fact)
                  (lambda (n)
                    (if (= n 0)
                        1
                        (* n (fact (- n 1))))))) 10))
   3628800)

  (test
   "Y-combinator2"
   (interp '((Y (lambda (sum)
                  (lambda (n)
                    (if (= n 0)
                        0
                        (+ n (sum (- n 1))))))) 10))
   55)

  )
(add-test-cases! test-interp-y-combinator)

; begin

(define (test-interp-begin)
  (test
   "begin1"
   (interp '(begin
              1
              2
              3))
   3)

  (test
   "begin2"
   (interp '(begin
              (define x 23)
              (set! x 25);
              x))
   25)

  (test
   "begin3"
   (interp '(begin
              (+ (begin
                   1
                   2)
                 (begin
                   3
                   4))))
   6)

  (test
   "begin4"
   (interp '(begin
              (define add1
                (lambda (a)
                  (+ a 1)))
              (define sub1
                (lambda (a)
                  (- a 1)))
              (+ (add1 4)
                 (sub1 6))))
   10)
  )
(add-test-cases! test-interp-begin)

; call/cc

(define (test-interp-callcc)
  (test
   "call/cc1"
   (interp '(+ 1
               (call/cc
                (lambda (k)
                  100))))
   101)

  (test
   "call/cc2"
   (interp '(+ 1
               (call/cc
                (lambda (k)
                  (+ 2 (k 10))))))
   11)

  (test
   "call/cc3"
   (interp '(+ 1
               (call/cc
                (lambda (k1)
                  (+ 2 (call/cc
                        (lambda (k2)
                          (+ 3 (k1 10)))))))))
   11)

  (test
   "call/cc4"
   (interp '(+ 1
               (call/cc
                (lambda (k1)
                  (+ 2 (call/cc
                        (lambda (k2)
                          (+ 3 (k2 10)))))))))
   13)

  )
(add-test-cases! test-interp-callcc)

; cond

(define (test-interp-cond)
  (test
   "cond1"
   (interp '(let ([x 2])
              (cond
                [(= x 3) 3]
                [(= x 1) 1]
                [(= x 0) 0]
                [(= x 2) "yes"]
                [else "else branch"])))
   "yes")

  (test
   "cond2"
   (interp '(let ([x 2])
              (cond
                [(= x 3) 3]
                [else "else branch"]
                [(= x 1) 1]
                [(= x 0) 0]
                [(= x 2) "yes"]
                [else "else branch1"]
                )))
   "else branch")

  (test
   "cond3"
   (interp '(let ([x 2])
              (cond
                [(= x 3) 3 4 5]
                [(= x 2) 4 5 6]
                [else "else branch"])))
                
   6)

  (test
   "cond4"
   (interp '(let ([x 2])
              (cond
                [(= x 3) 3 4 5]
                [(= x 2) (cond
                           [(= 1 2) "incorrect"]
                           [else "yes"])]
                [else "else branch"])))
                
   "yes")
  )
(add-test-cases! test-interp-cond)


; reset/shift

(define (test-interp-reset-shift)
  (test
   "reset-shift-1"
   (interp '(reset 2 3 4 5))
   5)

  (test
   "reset-shift-2"
   (interp '(+ 1
               (reset
                (+ 10
                   (shift k
                          (+ 100 200)
                          (k (k 5))
                          300)))))
   301)

  (test
   "reset-shift-3"
   (interp '(+ 1
               (reset
                (+ 10
                   (shift k
                          (+ 100 200)
                          (k (k 5))
                          )))))
   26)

  (test
   "reset-shift-4"
   (interp '(+ 1
               (reset
                (+ 10
                   (reset 
                    (* 2
                       (shift k
                              (+ 100 200)
                              (k (k 5))
                              300)))))))
   311)

  (test
   "reset-shift-5"
   (interp '(+ 1
               (reset
                (+ 10
                   (reset 
                    (* 2
                       (shift k
                              (+ 100 200)
                              (k (k 5))
                              )))))))
   31)

  (test
   "reset-shift-6"
   (interp '(begin
              (define c #f)
              (+ 1
                 (reset
                  (+ 2
                     (call/cc
                      (lambda (k)
                        (begin
                          (set! c k)
                          (* 50 (k 100)))))
                     (shift k (k 300)))))))
   403)

  (test
   "reset-shift-7"
   (interp '(+ 1
               (reset
                (+ 10
                   (shift k
                          (+ (k (k 300))
                             (k 300)))))))
   631)

  (test
   "reset-shift-8"
   (interp '(+ 1
               (reset
                (+ 1 (+ 1
                        (shift k
                               (+ (k 0)
                                  (k 0))))))))
   5)

  (test
   "reset-shift-9"
   (interp '(+ 1
               (reset (+ 1 (+ 1
                              (shift k (k 10)))))))
   13)
  
  )
(add-test-cases! test-interp-reset-shift)


; quote

(define (test-interp-quote)
  (test
   "quote1"
   (interp ''c)
   'c)

  (test
   "quote2"
   (interp '(if (= 1 1)
                'hello
                'world))
   'hello)

  (test
   "quote3"
   (interp '((lambda (x)
               '(1 2 3 4))
             100))
   '(1 2 3 4))

  )
(add-test-cases! test-interp-quote)

;; api

(test-all)
