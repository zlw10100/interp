#lang racket

(require "struct.rkt")
(require "env.rkt")

(define (make-native-env)
  (make-env/items
   `((+ . ,(native +))
     (- . ,(native -))
     (* . ,(native *))
     (/ . ,(native /))
     (< . ,(native <))
     (> . ,(native >))
     (<= . ,(native <=))
     (>= . ,(native >=))
     (= . ,(native =))
     (eq? . ,(native eq?))
     
     (println . ,(native println))
     (printf . ,(native printf))
     
     (cons . ,(native cons))
     (cons? . ,(native cons?))
     (pair? . ,(native pair?))
     (car . ,(native car))
     (cdr . ,(native cdr))
     (empty? . ,(native empty?))
     
     (list . ,(native list))
     (list . ,(native list))
     (list? . ,(native list?))
     (append . ,(native append))
     (string-append . ,(native string-append))
     (last . ,(native last))
     )))

(provide make-native-env)
