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
     (println . ,(native println))
     (printf . ,(native printf))
     )))

(provide make-native-env)
