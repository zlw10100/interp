#lang racket

(struct lookup-not-found ())
(provide lookup-not-found)
(provide lookup-not-found?)

(struct closure (params body env))
(provide closure)
(provide closure?)
(provide closure-params)
(provide closure-body)
(provide closure-env)

(struct native (procedure))
(provide native)
(provide native?)
(provide native-procedure)

(struct builtin (procedure))
(provide builtin)
(provide builtin?)
(provide builtin-procedure)

(struct continuation (procedure))
(provide continuation)
(provide continuation?)
(provide continuation-procedure)

; parial continuation
(struct pcontinuation (procedure))
(provide pcontinuation)
(provide pcontinuation?)
(provide pcontinuation-procedure)

(struct not-matched ())
(provide not-matched)
(provide not-matched?)

(struct not-bind ())
(provide not-bind)
(provide not-bind?)
