#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)
(provide (struct-out saw-proc) saw-λ)

(struct saw-proc (columns bindings procs))

(define-syntax (saw-λ stx)
  (syntax-parse stx
    [(_ [col:id (bound:id ...) body:expr ...] ...)
     #'(saw-proc (list (symbol->string (quote col)) ...)
                 (list (map symbol->string (list (quote bound) ...)) ...)
                 (list
                  (λ (bound ...)
                    body ...)
                  ...))]))
