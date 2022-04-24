#lang racket

(provide 
 (struct-out lookup-not-found)
 (struct-out closure)
 (struct-out native)
 (struct-out builtin)
 (struct-out continuation)
 (struct-out pcontinuation)
 (struct-out not-matched)
 (struct-out not-bind)
 (struct-out namespace)
 )

(struct lookup-not-found ())
(struct closure (params body env))
(struct native (procedure))
(struct builtin (procedure))
(struct continuation (procedure))
(struct pcontinuation (procedure))
(struct not-matched ())
(struct not-bind ())
(struct namespace (env))
