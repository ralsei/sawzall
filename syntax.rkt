#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)
(provide (struct-out column-proc)
         (for-syntax column-syntax-form))

(struct column-proc (columns bindings procs))

(define-for-syntax (column-syntax-form stx internal-function-stx)
  (syntax-parse stx
    [(_ frame:expr [col:id (bound:id ...) body:expr ...] ...)
     #:with internal-function internal-function-stx
     #'(internal-function frame
                          (column-proc (list (symbol->string 'col) ...)
                                       (list (map symbol->string (list 'bound ...)) ...)
                                       (list (λ (bound ...)
                                               body ...)
                                             ...)))]))
