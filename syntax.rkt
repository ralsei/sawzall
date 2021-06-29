#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)
(provide (struct-out column-proc)
         (for-syntax column-syntax-form))

(struct column-proc (columns bindings procs) #:transparent)

(define-for-syntax (column-syntax-form stx internal-function-stx faux-types?)
  (syntax-parse stx
    [(_ frame:expr [col:id ([bound:id (~literal :) ty:id] ...) body:expr ...] ...)
     #:with internal-function internal-function-stx
     (when (not faux-types?)
       (raise-syntax-error (syntax->datum (attribute internal-function))
                           "types should not be specified here"))
     #'(internal-function frame
                          (column-proc (list (symbol->string 'col) ...)
                                       (list (list (cons (symbol->string 'bound)
                                                         'ty)) ...
                                             ...)
                                       (list (λ (bound ...)
                                               body ...)
                                             ...)))]
    [(_ frame:expr [col:id (bound:id ...) body:expr ...] ...)
     #:with internal-function internal-function-stx
     #'(internal-function frame
                          (column-proc (list (symbol->string 'col) ...)
                                       (list (list (cons (symbol->string 'bound)
                                                         #f) ...)
                                             ...)
                                       (list (λ (bound ...)
                                               body ...)
                                             ...)))]))
