#lang racket/base
(require (for-syntax racket/base
                     racket/list)
         racket/contract/base
         syntax/parse/define)
(provide (struct-out column-proc)
         (struct-out row-proc)
         (struct-out sort-proc)
         (for-syntax column-syntax-form
                     row-syntax-form
                     sort-syntax-form))

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
     #:with fn-name (attribute internal-function)
     (when (and (not faux-types?)
                (andmap (λ (x) (and (syntax->datum x) #t)) (flatten (attribute binding.ty))))
       (raise-syntax-error (syntax->datum (attribute internal-function))
                           "types should not be specified here"))
     #'(internal-function
        frame
        (column-proc (list (symbol->string 'col) ...)
                     (list (list (cons (symbol->string 'binding.var)
                                       (contract (or/c 'element 'vector #f)
                                                 'binding.ty
                                                 'fn-name 'fn-name))
                                 ...) ...)
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

(struct sort-proc (columns comparators))

(define-for-syntax (sort-syntax-form stx internal-function-stx)
  (syntax-parse stx
    [(_ frame:expr [col:id body:expr ...] ...)
     #:with internal-function internal-function-stx
     #:with fn-name (attribute internal-function)
     #'(internal-function
        frame
        (sort-proc (list (symbol->string 'col) ...)
                   (list (contract (-> any/c any/c boolean?) (let () body ...)
                                   'fn-name 'fn-name)
                         ...)))]))
