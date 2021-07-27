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

Sawzall is a grammar for manipulating data, and provides a set of primitives that allow solving
common data-manipulation problems in a style similar to spreadsheets. Sawzall uses the
@racketmodname[data-frame] library as its primary object of input and output, and is designed to
manipulate these data-frames.

Sawzall is designed around @italic{tidy data}, but most data found in the wild will not be tidy.
For more information, see @secref{pivoting}.

Sawzall is designed with the @racketmodname[threading] library in mind. While it is possible to use
without it, most operations will be more natural expressed with @racket[~>].

Many examples in this documentation will be based around the following simple frame:
@examples[#:eval ev #:label #f
  (define example-df
    (row-df [grp trt adult juv]
             "a" "b" 1     10
             "a" "b" 2     20
             "b" "a" 3     30
             "b" "b" 4     40
             "b" "b" 5     50))
]

@table-of-contents[]

@section[#:tag "constructors"]{Constructing data-frames}

These are constructors for data-frames that may be more ergonomic than @racket[make-data-frame].
Generally, while doing data analysis, you want to use @racket[df-read/csv] or a similar function
to read in real-world data from some source. If you are constructing data from a Racket program,
however, these may be useful.

@defform[(column-df [column-name column-data] ...)
         #:contracts ([column-name (or/c identfier? string?)]
                      [column-data vector?])]{
  Constructs a data-frame with the given @racket[column-name]s, with each @racket[column-data]
  as the data in each column.

  @racket[column-name] can either be an identifier or an expression that evaluates to a string.
  If it is an identifier, it will be taken literally.

  @examples[#:eval ev
    (show (column-df [x (vector 1 2 3)]
                     [(string-append "a" "b") (vector "a" "b" "c")]))
  ]
}

@defform[(row-df [column-name ...] value ...)]{
  Constructs a data-frame with the given @racket[column-name]s, row-by-row with the given
  @racket[value]s.

  This method runs in quadratic time, but it is executed solely at compile time, so
  its performance should be roughly equivalent to that of @racket[column-df].
}

@section[#:tag "display"]{Displaying data}

These functions display data to the screen, for use in interactive data analysis. For @italic{saving}
data, see @racket[df-write/csv] et al.

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

The overwhelming majority of operations in Sawzall respect the "grouping" of a data-frame. Most
operations are done on groups defined by variables, so grouping takes an existing frame and converts
it into a grouped one, in which operations are performed by group.

@subsection{Grouping}
@defproc[(grouped-data-frame? [v any/c]) boolean?]{
  Determines if the input @racket[v] is a grouped data-frame. These can only be constructed by
  @racket[group-with], or as the result on another operation on an existing grouped data-frame.
}

@defproc[(group-with [df data-frame?] [var string?] ...) grouped-data-frame?]{
  Takes an existing data-frame @racket[df], and groups it with respect to the given variables
  @racket[var] sequentially, returning a grouped data frame.

  This does not change how the data-frame is displayed with @racket[show] or @racket[introspect], but
  the result is internally different, and cannot be used with regular data-frame operators like
  @racket[df-select].

  @examples[#:eval ev
    (~> example-df
        (group-with "grp" "trt")
        show)
  ]
}

@defproc[(ungroup [df (or/c data-frame? grouped-data-frame?)]) data-frame?]{
  Removes all levels of grouping from a grouped data frame, returning a singular data frame. In most cases,
  you'll want to do this before passing your wrangled data to some other application.

  If @racket[df] is not grouped, this does nothing.
}

@defproc[(ungroup-once [df (or/c data-frame? grouped-data-frame?)]) (or/c data-frame? grouped-data-frame?)]{
  Removes the last level of grouping from a grouped data frame. For example, if a grouped frame is grouped
  by X and Y, running @racket[ungroup-once] it would make it grouped by just X.

  If @racket[df] is not grouped, this does nothing.
}

@subsection{Splitting}

The following operations behave similar to the above counterparts, but they return a list instead of a
grouped data frame, so you must use @racket[map] to do sequential groups or perform operations.

These operations are also notably less performant due to the amount of copying involved.

@defproc[(split-with [df data-frame?] [var string?]) (listof data-frame?)]{
  Splits the given data-frame @racket[df] along the input variable @racket[var], returning a list of each
  possibility.
}

@defproc[(combine [df data-frame?] ...) data-frame?]{
  Appends the shared series of the input data-frames into a single data-frame.
}

@section[#:tag "where"]{Filtering}

This operation subsets a data frame, returning rows which satisfy a given condition.

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

@defform[(where* df (column-name ...) (match-pattern ...))
         #:contracts ([df (or/c data-frame? grouped-data-frame?)])]{
  Returns @racket[df], except only rows in which @racket[column-name] matches each @racket[match-pattern]
  is kept. See @racket[match].

  There must be exactly as many @racket[column-name]s as there are @racket[match-pattern]s.

  @examples[#:eval ev
    (~> example-df
        (where* (grp juv) ("b" (? (λ (x) (< x 50)) _)))
        show)
  ]
}

@section[#:tag "slice"]{Slicing}

This operation subsets a data frame, returning columns specified by a smaller expression language.

@defform/subs[#:literals (or and not containing ending-with starting-with everything all-in any-in
                          data-frame? grouped-data-frame? string? regexp? sequence/c)
              (slice df slice-spec)
              [(df (code:line data-frame?)
                   grouped-data-frame?)
               (slice-spec (code:line string)
                           [string-literal ...]
                           regexp
                           everything
                           (or slice-spec ...)
                           (and slice-spec ...)
                           (not slice-spec)
                           (all-in string-sequence)
                           (any-in string-sequence)
                           (starting-with string)
                           (ending-with string)
                           (containing string))
               (string (code:line string?))
               (regexp (code:line regexp?))
               (string-literal (code:line string?))
               (string-sequence (code:line (sequence/c string?)))]]{
  Constructs a new data-frame with columns from the input @racket[df], with columns specified
  by the evaluation of @racket[slice-spec].

  @racket[slice-spec] is an expression in a much smaller language.
  Values in this language are:
  @itemlist[
    @item{A @racket[string?], to select a single string.}
    @item{A @racket[regexp?], to select columns with names matching that regular expression.}
  ]
  The language has the following operators:
  @specsubform[[str ...]
               #:contracts ([str string?])]{
    Selects the columns with names @racket[str]. This doesn't have to be brackets, but it is
    recommended for readability's sake.

    The strings supplied here must be string literals, or else syntax errors would be too poor.
    If you want to use a variable, use @racket[all-in] or @racket[any-in].
  }
  @defsubform[#:id everything everything]{
    Selects every column in the given data-frame.
  }
  @defsubform[(or spec ...)]{
    Selects the union of the given @racket[spec]s.
  }
  @defsubform[(and spec ...)]{
    Selects the intersection of the given @racket[spec]s.
  }
  @defsubform[(not spec ...)]{
    Selects the complement of (everything but) the given @racket[spec]s.
  }
  @defsubform[(all-in sequence)
              #:contracts ([sequence (sequence/c string?)])]{
    Selects all variables with names in the given @racket[sequence]. If a name is present in
    @racket[sequence] but not the data-frame, this errors.

    The input sequence cannot be infinite, or this does not terminate.
  }
  @defsubform[(any-in sequence)
              #:contracts ([sequence (sequence/c string?)])]{
    Like @racket[all-in], but does not error when a name is not present in @racket[sequence], and
    merely does not select it.
  }
  @defsubform[(starting-with suffix)
              #:contracts ([prefix string?])]{
    Selects columns with names beginning with the given @racket[prefix].
  }
  @defsubform[(ending-with suffix)
              #:contracts ([suffix string?])]{
    Selects columns with names ending with the given @racket[suffix].
  }
  @defsubform[(containing substr)
              #:contracts ([substr string?])]{
    Selects columns with names containing the given @racket[substr].
  }

  Using these outside of the context of @racket[slice] is a syntax error (aside from
  @racket[and], @racket[or], and @racket[not], for obvious reasons).

  This operation will not remove variables that a grouped data frame is grouped by, as
  this would destroy group invariants.

  @examples[#:eval ev
    (~> example-df
        (slice "trt")
        show)
    (~> example-df
        (slice (not ["trt" "grp"]))
        show)
    (~> example-df
        (slice (containing "t"))
        show)
  ]
}

@section[#:tag "create"]{Creating and modifying columns}

These operations add new variables, preserving existing ones. This operation uses both vectorized and
regular operations.

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

This operation summarizes a data frame into a smaller one, using some kind of summary statistic,
based on a vectorized operation.

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

These operations join two tables in varying ways, along some common column or set of columns
(the "spine"). When multiple columns are specified, the data-set is joined along their
combinations. When none are specified, the spine defaults to all shared variables.

All joining operations ignore grouping. The grouping of the first argument will be preserved,
but grouping does not play a factor in how the operation performs (compared to a plain table).

The following example data-frames are used in this section:
@examples[#:eval ev #:label #f
  (define woodland1
    (row-df [site habitat]
             "b"  "grassland"
             "a"  "meadow"
             "c"  "woodland"))
  (define woodland2
    (row-df [site day catch]
             "c"  1   10
             "b"  1   12
             "c"  2   20
             "b"  2   24))
]

@subsection{Combining joins}

These joins combine variables from the two input data-frames.

@defproc[(left-join [df1 (or/c data-frame? grouped-data-frame?)]
                    [df2 (or/c data-frame? grouped-data-frame?)]
                    [by string?] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns a new data-frame, with all rows from @racket[df1], and all columns from @racket[df1]
  and @racket[df2]. Rows are compared by the value of the variable(s) @racket[by].

  If @racket[by] is not specified, it defaults to all the common columns in @racket[df1] and
  @racket[df2], in an unspecified order.

  Rows in @racket[df1] that have no corresponding value(s) of @racket[by] in @racket[df2] will have
  NA values in the new columns.

  Rows in @racket[df1] that have multiple corresponding values of @racket[by] in @racket[df2]
  will have all combinations of the @racket[df1] and @racket[df2] values in the result.

  @examples[#:eval ev
    (~> woodland1
        (left-join woodland2 "site")
        show)
    (~> woodland2
        (left-join woodland1 "site")
        show)
  ]
}

@defproc[(right-join [df1 (or/c data-frame? grouped-data-frame?)]
                     [df2 (or/c data-frame? grouped-data-frame?)]
                     [by string?] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns a new data-frame, with all rows from @racket[df2], and all columns from @racket[df1]
  and @racket[df2]. Rows are compared by the value of the variable(s) @racket[by].

  If @racket[by] is not specified, it defaults to all the common columns in @racket[df1] and
  @racket[df2], in an unspecified order.

  This is equivalent to @racket[(left-join df2 df1 by #:cmp? cmp?)].
}

@defproc[(inner-join [df1 (or/c data-frame? grouped-data-frame?)]
                     [df2 (or/c data-frame? grouped-data-frame?)]
                     [by string?] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns a new data-frame, with all rows from @racket[df1] with matching rows in @racket[df2],
  and columns of both @racket[df1] and @racket[df2]. Rows are compared by the value of the variable
  @racket[by].

  If @racket[by] is not specified, it defaults to all the common columns in @racket[df1] and
  @racket[df2], in an unspecified order.

  If there are multiple matches between the rows of @racket[df1] and @racket[df2], all combinations
  of the matches are returned.

  @examples[#:eval ev
    (~> woodland1
        (inner-join woodland2 "site")
        show)
  ]
}

@defproc[(full-join [df1 (or/c data-frame? grouped-data-frame?)]
                    [df2 (or/c data-frame? grouped-data-frame?)]
                    [by string?] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns a new data-frame, with all rows and columns from @racket[df1] and @racket[df2]. Rows are
  compared by the value of the variable @racket[by].

  If @racket[by] is not specified, it defaults to all the common columns in @racket[df1] and
  @racket[df2], in an unspecified order.

  If there is a row in @racket[df1] that does not have a value in @racket[df2], or vice versa, they
  will have NA values in the new columns.

  @examples[#:eval ev
    (~> woodland2
        (full-join woodland1 "site")
        show)
  ]
}

@section[#:tag "reorder"]{Sorting}

These operations are related to sorting data-frames.

Note that sorting a grouped data frame without respect to groups is not possible due to the way groups
function internally.

@defproc[(reorder [df (or/c data-frame? grouped-data-frame?)]
                  [column-spec (or/c string? (cons/c string? (-> any/c any/c boolean?)))]
                  ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns @racket[df], except sorted by a series of columns. Each @racket[column-spec] is either a column
  name, or a pair of a column name and a comparator.

  If no comparator is specified, it defaults to @racket[orderable<?], which will cause an ascending sort
  for most types of data.

  If a custom ordering is specified, it should be a strict ordering (so, if objects are @racket[equal?],
  the comparator should return false). Otherwise, @racket[group-with] and other operations involving
  binary search will behave unexpectedly.

  Note that reordering ensures that the first @racket[column-spec] is truly sorted. The subsequent
  @racket[column-spec]s are used to break the ties (i.e. two values are @racket[equal?]).

  This operation sorts within groups, if there are any. If you wish to sort ignoring groups, you will
  have to use @racket[ungroup] first. This is due to the way groups are managed internally.

  @examples[#:eval ev
    (~> example-df
        (reorder (cons "trt" string-ci>?)
                 (cons "adult" >))
        show)

    (~> example-df
        (group-with "trt")
        (reorder (cons "adult" >))
        show)
  ]
}

@defproc[(by-vector [vec vector?]) (-> any/c any/c boolean?)]{
  Returns a comparator based on the positions in the given input vector @racket[vec].

  This is useful for "replacing" a column in a frame, while retaining the order of observations.

  @examples[#:eval ev
    (~> example-df
        (reorder (cons "juv" (by-vector (vector 50 20 30 10 40))))
        show)
  ]
}

@defproc[(orderable? [v any/c]) boolean?]{
  Determines if the given type of value @racket[v] can be sorted against another @racket[orderable?]
  by @racket[orderable<?].

  @racket[v] must be one of @racket[boolean?], @racket[char?], @racket[real?], @racket[symbol?],
  @racket[keyword?], @racket[string?], @racket[null?], @racket[void?], or @racket[eof-object?].

  This does not guarantee that @racket[orderable<?] will succeed. For example, @racket[orderable<?] will
  error if given two @racket[void?]s.
}

@defproc[(orderable<? [a orderable?] [b orderable?]) boolean?]{
  A generic comparator to determine if @racket[a] is less than @racket[b].

  Used by default by joins and @racket[reorder].
}

@section[#:tag "pivoting"]{Pivoting}

The majority of data found in the wild will not be @italic{tidy}, and therefore not work with the above
operations. The goal of pivoting is to help make your data tidy.

Tidy data is data where:
@itemlist[
  @item{Every column is a variable.}
  @item{Every row is an observation.}
  @item{Every cell is a single value.}
]

So, for example, if you were to have a column corresponding to a value and not a variable (such as a site),
or a column corresponding to a "type" of observation, these operations would help.

@defform[(pivot-longer df slice-spec
                       #:names-to names-to #:values-to values-to)
         #:contracts ([df data-frame?] [names-to string?] [values-to string?])]{
  Returns a new data-frame that is the input @racket[df] pivoted "longer", so less columns,
  more rows. This is useful for tidying wide-form data.

  @racket[slice-spec] is an expression in the slice sub-language. See @secref{slice} for more information
  on this language. Any not column returned by evaluating @racket[slice-spec] will be brought along so that
  former observations line up, but its data will not be modified.

  All the names of every column selected by @racket[slice-spec] is brought into a new column with name
  @racket[names-to], and all the values are brought into a new column with name @racket[values-to].

  This function does not work with grouped data frames, as it has potential to destroy some internal
  invariants.

  @examples[#:eval ev
    (define wide-df
      (row-df [day hour a  b  c]
               1   10   97 84 55
               2   11   78 47 54))
    (~> wide-df
        (pivot-longer ["a" "b" "c"] #:names-to "site" #:values-to "catch")
        show)
  ]
}

@defproc[(pivot-wider [df data-frame?]
                      [#:names-from names-from string?]
                      [#:values-from values-from string?])
         data-frame?]{
  Returns a new data-frame that is the input @racket[df] pivoted "wider", so less rows, more columns.
  This is useful for putting data into "wide form", optimized for data entry for export into spreadsheet
  software like Excel, or for some convoluted tidying pipeline.

  @racket[names-from] is the column to create new columns from, and @racket[values-from] is the column to
  get the corresponding data from.

  If a value is not found in the long format data-frame, it will be replaced with "NA" (@racket[#f]).

  This function does not work with grouped data frames, as it has potential to destroy some internal
  invariants.

  @examples[#:eval ev
    (define long-df1
      (row-df [day grp val]
               1   "A" 10
               1   "B" 20
               2   "B" 30))
    (~> long-df1
        (pivot-wider #:names-from "grp" #:values-from "val")
        show)

    (define long-df2
      (row-df [day hour grp val]
               1   10   "a" 83
               1   10   "b" 78
               1   11   "a" 80
               1   11   "b" 105
               2   10   "a" 95
               2   10   "b" 77
               2   11   "a" 96
               2   11   "b" 99))
    (~> long-df2
        (pivot-wider #:names-from "grp" #:values-from "val")
        show)
  ]
}

@section[#:tag "separate"]{Separating variables}

One of the principles of tidy data (as discussed in @secref{pivoting}) is that every column represents
a single variable. So when a column represents two or more variables, or when a column has half of a
variable (which is more rare), that data is not tidy regardless of its pivoting.

These operations are designed to separate string variables into different columns, or unite two
variables into one column.

The following example data will be used in this section:
@examples[#:eval ev #:label #f
  (define to-separate
    (row-df [col]
            #f
            "a-b"
            "a-d"
            "b-c"
            "d-e"))
]

@defproc[(separate [df data-frame?]
                   [column-name string?]
                   [#:into into (non-empty-listof (or/c string? #f))]
                   [#:separator separator (or/c string?
                                                regexp?
                                                exact-nonnegative-integer?
                                                (listof exact-nonnegative-integer?))
                                          #px"[^[:alnum:]]+"]
                   [#:remove? remove? boolean? #t]
                   [#:fill fill (or/c 'left 'right) 'right])
         data-frame?]{
  Returns a new data-frame with the same data as @racket[df], except the contents of
  @racket[column-name] are split into separate variables according to @racket[separator], with
  the names specified by @racket[into] for each split of the result.

  The variable stored in @racket[column-name] must be a string variable (alongside NA values).

  @racket[into] specifies what variables (index-wise) the result should be placed into. If
  @racket[#f] is placed in @racket[into], that result of the split is skipped.

  @racket[separator] is either a string, a regular expression, a number, or a list of numbers.
  @itemlist[
    @item{If @racket[separator] is a string or regular expression, @racket[regexp-split] is used
          to split up the values of @racket[column-name].}
    @item{If @racket[separator] is a number, the string is split into two pieces along that
          index into the string.}
    @item{If @racket[separator] is a list of numbers, the string is split into multiple pieces
          along those indices.}
  ]
  The default separator splits along all non-alphanumeric characters.

  If there are more results of a split than there are variables in @racket[into], the excess variables
  are dropped.

  If there are less results of a split than there are variables in @racket[into], NAs are inserted to
  the rightmost variables in the list if @racket[fill] is @racket['right], and to the leftmost if it is
  @racket['left].

  If @racket[remove?] is true, @racket[column-name] will not be present in the output data-frame.

  This function does not worked on grouped data frames, due to its potential to destroy group invariants.

  @examples[#:eval ev
    (~> to-separate
        (separate "col" #:into '("A" "B"))
        show)
    (~> to-separate
        (separate "col" #:into '("A" #f) #:remove? #f)
        show)
    (~> to-separate
        (separate "col" #:into '("A" "B") #:separator 1)
        show)
  ]
}

@defproc[(extract [df data-frame?]
                  [column-name string?]
                  [#:into into (non-empty-listof (or/c string? #f))]
                  [#:regex regex regexp? #px"([[:alnum:]]+)"]
                  [#:remove? remove? boolean? #t])
         data-frame?]{
  Like @racket[separate], but uses regular expression capturing groups, rather than splitting the
  contents of @racket[column-name].

  The variable stored in @racket[column-name] must be a string variable (alongside NA values).

  @racket[regex] is a regular expression with capturing groups. @racket[regex] is expected to
  have as many or less capturing groups (excluding group 0) as @racket[into] has variables. By
  default, this captures the first alphanumeric sequence.

  If the regex has no match, NA will appear in the output columns.

  If @racket[remove?] is true, @racket[column-name] will not be present in the output data-frame.

  This function does not work on grouped data frames, due to its potential to destroy group invariants.

  @examples[#:eval ev
    (~> to-separate
        (extract "col" #:into '("A"))
        show)
    (~> to-separate
        (extract "col"
                 #:into '("A" "B")
                 #:regex #px"([a-d]+)-([a-d]+)")
        show)
  ]
}

@defproc[(unite [df data-frame?]
                [column-name string?]
                [#:from from (non-empty-listof string?)]
                [#:combine combine-fn
                           (-> any/c ... any/c)
                           (λ args (string-join (filter (λ (x) x) args) "_"))]
                [#:remove? remove? boolean? #t])
         data-frame?]{
  The inverse transformation to @racket[separate]. Takes the variables in @racket[from] in @racket[df],
  and combines them using @racket[combine-fn] into a new column @racket[column-name].

  By default, @racket[combine-fn] takes each present variable, and inserts an underscore between them.

  If @racket[remove?] is true, all columns in @racket[from] will not be present in the output data-frame.

  This function does not work on grouped data frames, due to its potential to destroy group invariants.

  @examples[#:eval ev
    (define to-unite
      (row-df [str-a str-b num-a num-b]
              #f     "c"   13    90
              "a"    "b"   19    20
              "b"    #f    50    90
              "e"    "d"   59    25))
    (~> to-unite
        (unite "str" #:from '("str-a" "str-b"))
        (unite "num" #:from '("num-a" "num-b") #:combine +)
        show)
  ]
}

@section[#:tag "missing-values"]{Missing values}

Occasionally, a data-set will have a literal missing value. While reading data into a data-frame,
this can be specified with @racket[df-read/csv]'s @racket[#:na] argument; but usually, this value
ends up being @racket[#f].

These operations are designed to handle missing values, either by replacing them or dropping them.

The following example data-frame will be used in this section:
@examples[#:eval ev #:label #f
  (define has-some-na
    (row-df [col-a col-b]
            1      #f
            #f     2
            3      4))
]

@defproc[(replace-na [df (or/c data-frame? grouped-data-frame?)]
                     [column-name string?] [replace-with any/c] ...)
         (or/c data-frame? grouped-data-frame?)]{
  Returns a new data-frame with NA values in @racket[df] replaced with another value.
  In each @racket[column-name], NA is replaced with @racket[replace-with].

  @examples[#:eval ev
    (~> has-some-na
        (replace-na "col-a" 999)
        show)
    (~> has-some-na
        (replace-na "col-a" 999
                    "col-b" 333)
        show)
  ]
}

@defform[(drop-na df slice-spec)
         #:contracts ([df (or/c data-frame? grouped-data-frame?)])]{
  Returns a new data-frame like @racket[df], except with NA values removed from columns
  specified by the evaluation of @racket[slice-spec].

  The evaluation of @racket[slice-spec] determines what columns to remove NA values from.
  For documentation on this language, see @secref{slice}.

  @examples[#:eval ev
    (~> has-some-na
        (drop-na everything)
        show)
    (~> has-some-na
        (drop-na "col-a")
        show)
  ]
}
