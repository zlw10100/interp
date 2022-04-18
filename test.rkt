#lang racket

(require racket/exn)

(require "interp.rkt")

(struct not-raise ())

;; test
(define (test title x expected)
  (if (equal? x expected)
      (printf "[OK] test: ~a~n~n" title)
      (printf "[FAIL] test: ~a~nexpected: ~a~ngot: ~a~n~n" title expected x)))

(define (test-exn-string title exp expected)
  (let ([result (get-exn-string exp)])
    (cond
      [(not-raise? result)
       (printf "[FAIL] test: ~a~n" title)
       (printf "[WARN] expected exception is not raised!~n")
       ]
      [(equal? result expected)
       (printf "[OK] test: ~a~n~n" title)
       ]
      [else
       (printf "[FAIL] test: ~a~nexpected: ~a~ngot: ~a~n~n" title expected result)
       ])))

(define (get-exn-string exp)
  (with-handlers
      ([exn:fail?
        (lambda (exn)
          (exn->string exn))])
    (begin
      (interp exp)
      (not-raise))))
  
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


; test letrec

(define (test-interp-letrec)
  (test
   "letrec1"
   (interp '(letrec ([x 2]
                     [y (+ x 1)])
              (+ x y)))
   5)

  (test
   "letrec2"
   (interp '(letrec ([sum (lambda (n)
                            (if (= n 0)
                                0
                                (+ n (sum (- n 1)))))])
              (sum 10)))
   55)

  (test
   "letrec3"
   (interp '(letrec ([sum<5 (lambda (n)
                              (cond
                                [(= n 0) 0]
                                [(< n 5) (+ n (sum<5 (- n 1)))]
                                [else (sum>=5 n)]))]
                     [sum>=5 (lambda (n)
                               (cond
                                 [(= n 0) 0]
                                 [(>= n 5) (+ n (sum>=5 (- n 1)))]
                                 [else (sum<5 n)]))])
              (sum>=5 100)))
   5050)

  (test
   "letrec4"
   (interp '(letrec ([sum<5 (lambda (n)
                              (cond
                                [(= n 0) 0]
                                [(< n 5) (+ n (sum<5 (- n 1)))]
                                [else (sum>=5 n)]))]
                     [sum>=5 (lambda (n)
                               (cond
                                 [(= n 0) 0]
                                 [(>= n 5) (+ n (sum>=5 (- n 1)))]
                                 [else (sum<5 n)]))])
              (sum>=5 100)))
   5050)

  (test-exn-string
   "letrec5"
   '(letrec ([x 2]
             [x 3])
      x)
   "duplicate identifier in:  'x\n")
  
  )
(add-test-cases! test-interp-letrec)

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
; '

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


; quasiquote
; `

(define (test-interp-quasiquote)
  (test
   "quasiquote1"
   (interp '`c)
   'c)

  (test
   "quasiquote2"
   (interp '(if (= 1 1)
                `hello
                `world))
   'hello)

  (test
   "quasiquote3"
   (interp '((lambda (x)
               `(1 2 3 4))
             100))
   '(1 2 3 4))

  (test
   "quasiquote4"
   (interp '((lambda (x)
               `(1 2 3 x))
             100))
   '(1 2 3 x))

  (test
   "quasiquote5"
   (interp '((lambda (x)
               `(1 2 3 ,x))
             100))
   '(1 2 3 100))

  (test
   "quasiquote6"
   (interp '((lambda (x)
               `(1 ,x 3 ,x))
             100))
   '(1 100 3 100))

  (test
   "quasiquote7"
   (interp '((lambda (x)
               `(1 ,(+ x 1) 3 ,x))
             100))
   '(1 101 3 100))

  (test
   "quasiquote8"
   (interp '((lambda (x)
               `(1 ,((lambda (y)
                       (+ y 1)) x) 3 ,x))
             100))
   '(1 101 3 100))

  (test
   "quasiquote9"
   (interp '`())
   '())

  (test
   "quasiquote10"
   (interp '(let ([x 100])
              `(x ,x)))
   '(x 100))

  (test
   "quasiquote11"
   (interp '(let ([x 100])
              `(x ,(begin
                     (set! x 66)
                     x) ,x)))
   '(x 66 66))

  (test
   "quasiquote12"
   (interp '(let ([x 200])
              `(out ,(let ([x 100])
                       `(x ,(begin
                              (set! x 66)
                              x) ,x)) ,x)))
   '(out (x 66 66) 200))

  (test
   "quasiquote13"
   (interp '(let ([x '(1 2 3)])
              `(out ,x)))
   '(out (1 2 3)))
  
  (test
   "quasiquote14"
   (interp '(let ([x '(1 2 3)])
              `(out ,@x)))
   '(out 1 2 3))

  (test
   "quasiquote15"
   (interp '(let ([x '(1 2 3)]
                  [y '(a b c)])
              `(out (5 6) ,@x ,y)))
   '(out (5 6) 1 2 3 (a b c)))

  (test
   "quasiquote16"
   (interp '(let ([x '(1 2 3)]
                  [y '(a b c)])
              `(out (5 6) (7 ,y ,@x))))
   '(out (5 6) (7 (a b c) 1 2 3)))

  (test
   "quasiquote17"
   (interp '(let ([x 3])
              `(1 2 ,@x)))
   '(1 2 . 3))
  
  )
(add-test-cases! test-interp-quasiquote)


; match

(define (test-interp-match)
  (test
   "match1"
   (interp '(match 1
              [1 'yes]
              [_ 'no]))
   'yes)

  (test
   "match2"
   (interp '(match #t
              [#f 'yes]
              [_ 'no]))
   'no)

  (test
   "match3"
   (interp '(match #t
              [_ 'first]
              [#f 'yes]
              [_ 'no]))
   'first)
  
  (test
   "match4"
   (interp '(match "hello"
              ["world" 'incorrect]
              [#f 'incorrect1]
              ['hello 'incorrect-symbol]
              ["hello" 'correct]
              [_ 'incorrect-else]))
   'correct)

  (test
   "match5"
   (interp '(match 'hello
              ["world" 'incorrect]
              [#f 'incorrect1]
              ['hello 'correct]
              ["hello" 'incorrect-string]
              [_ 'incorrect-else]))
   'correct)

  (test
   "match6"
   (interp '(match '(1 2 3)
              [a a]
              [_ 'incorrect-else]))
   '(1 2 3))

  (test
   "match7"
   (interp '(match '(1 2 3)
              [(list 1 2 3) 'yes]
              [_ 'incorrect-else]))
   'yes)

  (test
   "match8"
   (interp '(match '(1 2 3)
              [(list 1 2 10) 'list]
              [`(1 2 3) 'yes]
              [_ 'incorrect-else]))
   'yes)

  (test
   "match9"
   (interp '(match '(1 2 (3 4) 5)
              [(list 1 2 `(3 4) 5) 'yes]
              [_ 'incorrect-else]))
   'yes)

  (test
   "match10"
   (interp '(match '(1 2 (3 4) 5)
              [(list 1 b `(3 4) c) (+ b c)]
              [_ 'incorrect-else]))
   7)

  (test
   "match11"
   (interp '(match '(1 2 (3 4) 5)
              [(list 1 2 c d) `(,d ,@c)]
              [_ 'incorrect-else]))
   '(5 3 4))

  (test
   "match12"
   (interp '(match '(1 2 (3 4) 5)
              [`(1 2 ,c ,d) `(,d ,@c)]
              [_ 'incorrect-else]))
   '(5 3 4))

  (test
   "match13"
   (interp '(match '(1 2 (3 4) 5)
              [`(1 2 ,@a ,d) `(,d ,a)]
              [_ 'incorrect-else]))
   '(5 ((3 4))))

  
  (test
   "match14"
   (interp '(match '(1 2 (3 4) 5)
              [`(1 2 (,x ,y) ,z)
               (begin
                 (let ([a 100])
                   (+ a x y z)))]
              [_ 'incorrect-else]))
   112)

  (test
   "match15"
   (interp '(begin
              (define a 100)
              (match '(1 2 (3 4) 5)
                [`(1 2 (,x ,y) ,z)
                 (begin
                   (set! a 66)
                   (+ a x y z))]
                [_ 'incorrect-else])))
   78)

  (test
   "match16"
   (interp '(begin
              (define a 100)
              (match '(1 2 (3 4) 5)
                [`(1 2 (,x ,y) ,z)
                 (begin
                   (set! a 66)
                   (+ a x y z))]
                [_ 'incorrect-else])
              (+ 1 a)))
   67)

  (test
   "match17"
   (interp '(match '(1 2 3)
              [`(1 2 ,@x) x]))
   '(3))

  )
(add-test-cases! test-interp-match)

;; api


; native

(define (test-interp-native)
  (test
   "native-cons"
   (interp '(cons 1 2))
   '(1 . 2))

  (test
   "native-cons1"
   (interp '(cons 1 '()))
   '(1))

  (test
   "native-car"
   (interp '(car (cons 1 2)))
   1)
  
  (test
   "native-cdr"
   (interp '(cdr (cons 1 2)))
   2)

  (test
   "native-list"
   (interp '(list 1 2 3 4 5))
   '(1 2 3 4 5))

  (test
   "native-last"
   (interp '(last (list 1 2 3 4 5)))
   5)

  (test
   "native-append"
   (interp '(append
             '(1 2 3)
             '(4 5)
             '(6)))
   '(1 2 3 4 5 6))

  (test
   "native-string-append"
   (interp '(string-append
             "hello"
             " world"
             "!"))
   "hello world!")
  
  )
(add-test-cases! test-interp-native)


(test-all)
