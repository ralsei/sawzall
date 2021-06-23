#lang racket
(require data-frame
         "saw-lambda.rkt")

(define (create df proc)
  (match-define (saw-proc new-cols binders procs) proc)
  (define return-df (df-shallow-copy df)) ; UNDOCUMENTED
  return-df)
