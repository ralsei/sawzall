#lang racket/base
(require (for-syntax racket/base
                     racket/list)
         syntax/parse/define)
(provide (struct-out column-proc)
         (struct-out row-proc)
         (for-syntax column-syntax-form
                     row-syntax-form))

(struct column-proc (columns bindings procs) #:transparent)

(begin-for-syntax
  (define-syntax-class binder
    #:attributes (var ty)
    [pattern var:id #:attr ty (syntax #f)]
    [pattern [var:id {~literal :} ty:id]]))

(define-for-syntax (column-syntax-form stx internal-function-stx faux-types?)
  (syntax-parse stx
    [(_ frame:expr [col:id (binding:binder ...) body:expr ...] ...)
     #:with internal-function internal-function-stx
     (when (and (not faux-types?)
                (andmap (λ (x) (and (syntax->datum x) #t)) (flatten (attribute binding.ty))))
       (raise-syntax-error (syntax->datum (attribute internal-function))
                           "types should not be specified here"))
     #'(internal-function
        frame
        (column-proc (list (symbol->string 'col) ...)
                     (list (list (cons (symbol->string 'binding.var)
                                       'binding.ty) ...) ...)
                     (list (λ (binding.var ...)
                             body ...)
                           ...)))]))

(struct row-proc (bindings proc))

(define-for-syntax (row-syntax-form stx internal-function-stx)
  (syntax-parse stx
    [(_ frame:expr (bound:id ...) body:expr ...)
     #:with internal-function internal-function-stx
     #'(internal-function
        frame
        (row-proc (list (symbol->string 'bound) ...)
                  (λ (bound ...)
                    body ...)))]))
