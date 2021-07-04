#lang scribble/manual
@(require scribble/example (for-label (except-in racket rename)
                                      data-frame sawzall threading))

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(require data-frame threading sawzall racket/vector))
     eval))

@title{Sawzall: A grammar for chopping up data}
@author{@(author+email "Hazel Levine" "hazel@knightsofthelambdacalcul.us")}

@defmodule[sawzall]

@bold{TODO:} needs examples, hard to find ones that make sense...?
current examples pilfered from Chez

Sawzall is a grammar for manipulating data, and provides a set of primitives that allow solving
common data-manipulation problems in a style similar to spreadsheets. Sawzall uses the
@racketmodname[data-frame] library as its primary object of input and output, and is designed to
manipulate these data-frames.

Sawzall is designed with the @racketmodname[threading] library in mind. While it is possible to use
without it, most operations will be more natural expressed with @racket[~>].

Many examples in this documentation will be based around the following simple frame:
@examples[#:eval ev #:label #f
  (define example-df
    (for/data-frame (grp trt adult juv)
                    ([grp-val (in-list (list "a" "a" "b" "b" "b"))]
                     [trt-val (in-list (list "a" "b" "a" "b" "b"))]
                     [adult-val (in-inclusive-range 1 5)]
                     [juv-val (in-inclusive-range 10 50 10)])
      (values grp-val trt-val adult-val juv-val)))
]

@table-of-contents[]

@section[#:tag "display"]{Displaying data}

@defproc[(show [df (or/c data-frame? grouped-data-frame?)] [#:all? all? boolean? #f]) void?]{
  Displays a data-frame @racket[df], alongside with grouping information (if it exists).
  Designed solely for interactive use. Also see @racket[df-describe], which presents summary statistics
  about a given data-frame.

  By default, six rows and six columns are shown (along with the names of each column), and the rest
  are elided. The columns shown are an in an unspecified order (as is @racket[df-series-names]). To
  display the entirety of @racket[df], pass @racket[#:all #t]: but this is likely to be overwhelming
  for large data-frames.

  @examples[#:eval ev
    (show example-df)
  ]
}

@defproc[(introspect [df (or/c data-frame? grouped-data-frame?)] [#:all? all? boolean? #f])
         (or/c data-frame? grouped-data-frame?)]{
  Like @racket[show], but returns its input. Useful for looking at the intermediate frame in a
  @racket[~>] chain, and then continuing processing.
}

@section[#:tag "grouping"]{Grouping and splitting}

@subsection{Grouping}

The overwhelming majority of operations in Sawzall respect the "grouping" of a data-frame. Most
operations are done on groups defined by variables, so grouping takes an existing frame and converts
it into a grouped one, in which operations are performed by group.

@defproc[(grouped-data-frame? [v any/c]) boolean?]{
  Determines if the input @racket[v] is a grouped data-frame. These can only be constructed by
  @racket[group-with], or as the result on another operation on an existing grouped data-frame.
}

@defproc[(group-with [df data-frame?] [var string?] ...) grouped-data-frame?]{
  Takes an existing data-frame @racket[df], and groups it with respect to the given variables
  @racket[var] sequentially, returning a grouped data frame.

  This does not change how the data-frame is displayed with @racket[show] or @racket[introspect], but
  the result is internally different, and cannot be used with regualr data-frame operators like
  @racket[df-select].

  @examples[#:eval ev
    (~> example-df
        (group-with "grp" "trt")
        show)
  ]
}

@defproc[(ungroup [df (or/c data-frame? grouped-data-frame?)]) (or/c data-frame? grouped-data-frame?)]{
  Removes the last level of grouping from a grouped data frame. For example, if a grouped frame is grouped
  by X and Y, ungrouping it would make it grouped by just X.

  If @racket[df] is not grouped, this does nothing.
}

@defproc[(ungroup-all [df (or/c data-frame? grouped-data-frame?)]) data-frame?]{
  Removes all levels of grouping from a grouped data frame, returning a singular data frame. In most cases,
  you'll want to do this before passing your wrangled data to some other application.

  If @racket[df] is not grouped, this does nothing.
}

@subsection{Splitting}

The following operations behave similar to the above counterparts, but they return a list instead of a
grouped data frame, so you must use @racket[map] to do sequential groups or perform operations.

@defproc[(split-with [df data-frame?] [var string?]) (listof data-frame?)]{
  Splits the given data-frame @racket[df] along the input variable @racket[var], returning a list of each
  possibility.
}

@defproc[(combine [df data-frame?] ...) data-frame?]{
  Appends the shared series of the input data-frames into a single data-frame.
}

@section[#:tag "where"]{Filtering}

@defform[(where df (bound-column ...) body ...)
         #:contracts ([df (or/c data-frame? grouped-data-frame?)])]{
  Returns @racket[df], except only rows in which @racket[body] returns true are kept.

  The bound variables in @racket[body] are values of the given @racket[bound-column]s. The frame is
  iterated upon, and for each row, @racket[body] is checked with the given bound variables.

  @examples[#:eval ev
    (~> example-df
        (where (adult) (> adult 3))
        show)
    (~> example-df
        (where (grp juv) (and (string=? grp "b") (< juv 50)))
        show)
  ]
}

@section[#:tag "create"]{Creating and modifying columns}

@defform/subs[#:literals (vector : element data-frame? grouped-data-frame?)
              (create df [new-column (binder ...) body ...] ...)
              [(df (code:line data-frame?)
                   grouped-data-frame?)
               (binder (code:line bound-column)
                       [bound-column : type])
               (type (code:line element)
                     vector)]]{
  Returns @racket[df], except with a derived column, or multiple derived columns. If the given column is
  already present in the data-frame, it will be (immutably) overridden, and otherwise it will be created.

  Each new column is specified by a single clause. The column created will have the name @racket[new-column],
  and be specified by the expressions in @racket[body].

  The bound variables in @racket[body] are specified by @racket[binder]. Each bound variable either has the type
  @racket[element], which binds a single element of the given column and maps over it, or @racket[vector],
  which binds the entire column. If a type for a bound variable is not specified, it defaults to
  @racket[element].

  If every bound variable in a given column specification is of type @racket[vector], it is expected that
  @racket[body] produces a vector of the same length as all other columns. Otherwise, it is expected that
  @racket[body] produces some quantity, and it will be mapped over every column specified by variables of type
  @racket[element].

  @examples[#:eval ev
    (define (v/ vec c) (vector-map (λ (v) (/ v c)) vec))
    (define (sum vec)
      (for/sum ([v (in-vector vec)])
        v))

    (~> example-df
        (create [total (adult juv) (+ adult juv)]
                [grp (grp) (string-append "blerg" grp)]
                [freq ([juv : vector]) (v/ juv (sum juv))])
        show)
  ]
}

@defproc[(rename [df (or/c data-frame? grouped-data-frame?)]
                 [from string?] [to string?] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns @racket[df], except with each column with name @racket[from] renamed to @racket[to].

  @examples[#:eval ev
    (~> example-df
        (rename "grp" "waldo"
                "trt" "warbly")
        show)
  ]
}

@section[#:tag "aggregate"]{Summarizing}

@defform[(aggregate df [new-column (bound-column ...) body ...] ...)
         #:contracts ([df (or/c data-frame? grouped-data-frame?)])]{
  Creates a new frame with the grouping information (if any) and new aggregated columns as specified. The
  new data-frame will have only columns corresponding to the groups of the frame and the new derived columns.
  It is likely you want to use @racket[group-with] first (or else you'll end up with just a single value).

  Each new column is specified by a single clause. The column created will have the name @racket[new-column],
  and be specified by the expressions in @racket[body].

  The bound variables in @racket[body] are specified by @racket[bound-column]. Unlike @racket[create], all
  variables bound in @racket[body] are the entirety of the column as a vector. @racket[body] is expected to
  produce a single value, which is the "aggregation" of that vector.

  If the input is a grouped data-frame, the last layer of grouping will be implicitly removed after
  aggregating.

  @examples[#:eval ev
    (~> example-df
        (aggregate [sum (adult) (vector-length adult)])
        show)
    (~> example-df
        (group-with "grp")
        (aggregate [adult-sum (adult) (sum adult)]
                   [juv-sum (juv) (sum juv)])
        show)
    (~> example-df
        (group-with "grp" "trt")
        (aggregate [adult-sum (adult) (sum adult)]
                   [juv-sum (juv) (sum juv)])
        show)
  ]
}

@section[#:tag "join"]{Joining}

@bold{Unimplemented.}

@section[#:tag "reorder"]{Sorting}

@bold{Implemented, but undocumented.}

@section[#:tag "reshaping"]{Reshaping}

@bold{Unimplemented.}
