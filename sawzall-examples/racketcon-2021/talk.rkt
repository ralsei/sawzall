#lang at-exp slideshow
(require colormaps
         data-frame
         file/gunzip
         graphite
         math/statistics
         (except-in pict/conditional show)
         (prefix-in pict: pict/conditional)
         pict/flash
         pict/shadow
         ppict/2
         ppict/slideshow2
         sawzall
         slideshow/code
         slideshow/staged-slide
         slideshow-text-style
         threading)

(define *global-font* "Source Sans Pro")
(define *mono-font* "Julia Mono")

(current-main-font *global-font*)
(current-code-font *mono-font*)
(get-current-code-font-size (thunk 20)) ;; ???

(sawzall-show-formatter
 (λ (val)
   (if (rational? val)
       (~r val #:precision 3)
       (~a val))))

;;;; helper functions
(define (frame p)
  (refocus (cc-superimpose
            p
            (rounded-rectangle (+ (pict-width p) 10)
                               (+ (pict-height p) 10)
                               #:border-width 3
                               #:border-color "blue"))
           p))

(define-syntax-rule (pslide/staged [name ...] arg ...)
  (staged [name ...] (pslide arg ...)))

(define (authors whos where)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([author-name #:size 30 #:color "blue"]
     [institution #:size 20])

    (vc-append (current-line-sep)
               (apply hc-append 50
                      (for/list ([who (in-list whos)])
                        (colorize (author-name who) "blue")))
               (blank (/ (current-font-size) 3))
               (scale/improve-new-text (institution where) 0.8))))

(define (take* lst n)
  (if (> n (length lst))
      lst
      (take lst n)))

(define (make-bang radius [text ""])
  (define bang (cc-superimpose (colorize (filled-flash radius radius) "red")
                               (colorize (filled-flash (- radius (/ radius 5)) (- radius (/ radius 5))) "orange")))
  (with-text-style
    #:defaults [#:face *global-font*]
    ([bang-t #:size (round (/ radius 6))])
    (cc-superimpose bang (bang-t text))))

;; do we need to specify what these are?
;; we can probably just say it verbally
(define (v/ vec c) (for/vector ([v (in-vector vec)]) (if v (/ v c) #f)))
(define (sum vec) (for/sum ([v (in-vector vec)] #:when v) v))

(define (df-show-slide df name csv-file [lst #f])
  (define defined-to (string->symbol (string-downcase name)))

  (with-text-style
    #:defaults [#:face *global-font*]
    ()

    (pslide
     #:name (string-append "Dataframe: " name)
     #:go (coord 0.05 0.05 'lt)
     (code
      (define #,defined-to
        (df-read/csv #,(string-append "../data/" csv-file)
                     #:na "NA")))
     #:go (coord 0.95 0.95 'rb)
     (show-pict df lst #:font-size 23))))

(define (show-pict df [lst #f] #:font-size [font-size 20] #:no-header? [no-header? #f])
  (define to-show
    (if (not lst)
        (take* (df-series-names df) 6)
        lst))
  (define described (string-split (with-output-to-string (thunk (show df (all-in to-show)))) "\n"))
  (define lines
    (if no-header?
        (drop described 1)
        described))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([mono #:size font-size #:face *mono-font*])
    (apply vl-append (current-line-sep)
           (for/list ([d (in-list lines)]
                      [_ (in-range (if no-header? 15 16))]) ; hack to hide help text
             (mono d)))))

;;;; data
;;;; likely that some of this is unused by the time the talk rolls around
(define gapminder (df-read/csv "../data/all_gapminder.csv" #:na "NA"))
(df-del-series! gapminder "")
(define oecd (df-read/csv "../data/oecd.csv" #:na "NA"))
(df-del-series! oecd "")
(define gss-sm (rename (df-read/csv "../data/gss_sm.csv" #:na "NA") "" "id"))
(define billboard (df-read/csv "../data/billboard.csv" #:na "NA"))
(define organdata (df-read/csv "../data/organdata.csv" #:na "NA"))
(define midwest (df-read/csv "../data/midwest.csv" #:na "NA"))
(define anscombe
  (row-df [x1 x2 x3 x4 y1    y2   y3    y4]
          10  10 10 8  8.04  9.14 7.46  6.58
          8   8  8  8  6.95  8.14 6.77  5.76
          13  13 13 8  7.58  8.74 12.74 7.71
          9   9  9  8  8.81  8.77 7.11  8.84
          11  11 11 8  8.33  9.26 7.81  8.47
          14  14 14 8  9.96  8.10 8.84  7.04
          6   6  6  8  7.24  6.13 6.08  5.25
          4   4  4  19 4.26  3.10 5.39  12.50
          12  12 12 8  10.84 9.13 8.15  5.56
          7   7  7  8  4.82  7.26 6.42  7.91
          5   5  5  8  5.68  4.74 5.73  6.89))
(define flights
  (let ()
    (define data
      (call-with-output-string
       (λ (out)
         (call-with-input-file "../data/flights.csv.gz"
           (λ (in) (gunzip-through-ports in out))))))
    (call-with-input-string data (curry df-read/csv #:na "NA"))))

;;;; actual slides
(define (title-slide)
  (define reciprocating-saw
    (scale-to-fit (bitmap "reciprocating_saw.jpg") (get-client-w) (get-client-h)))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([heading #:size 50 #:bold? #t])

    (pslide
     #:go (coord 0 0.5 'lc)
     reciprocating-saw
     #:go (coord 0.55 0.05 'rt)
     (rotate (make-bang 250 "✨ new ✨") (/ pi 6))
     #:go (coord 0.75 0.5 'cc)
     (vc-append
      (current-line-sep)
      @heading{Slicing Data with Sawzall}
      (authors '("Hazel Levine") "Indiana University")))))

(define (sawzall-intro-slides)
  (define dplyr-logo (scale-to-fit (bitmap "dplyr-logo.png") 200 200))
  (define tidyr-logo (scale-to-fit (bitmap "tidyr-logo.png") 200 200))
  (define racket-logo (scale-to-fit (bitmap "racket-logo.png") 200 200))

  (define bang (make-bang 100))
  (define dplyr+tidyr+bang (vc-append 50 (hc-append 30 dplyr-logo tidyr-logo) bang))
  (define dplyr+tidyr->bang
    (pin-arrow-line 30 (pin-arrow-line 30 dplyr+tidyr+bang
                                       dplyr-logo cc-find
                                       bang ct-find
                                       #:under? #t
                                       #:line-width 3
                                       #:color "cadet blue")
                    tidyr-logo cc-find
                    bang ct-find
                    #:under? #t
                    #:line-width 3
                    #:color "cadet blue"))
  (define dplyr+tidyr->bang->racket
    (pin-arrow-line 30 (vc-append 50 dplyr+tidyr->bang racket-logo)
                    dplyr+tidyr->bang cb-find
                    racket-logo ct-find
                    #:line-width 3
                    #:color "cadet blue"))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 15 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What is Sawzall?}
     #:go (coord 0.05 0.5 'lc)
     (vl-append
      (current-line-sep)
      @ti{a Racket library for data manipulation}
      @ti{like map/filter/fold, but for tabular data}
      @ti{designed to be compositional, with the @tt{~>} operator}
      @ti{inspired by R's dplyr+tidyr and Julia's DataFrames.jl})
     #:go (coord 0.75 0.5 'cc)
     dplyr+tidyr->bang->racket)

    (pslide/staged
     [none one two three]
     #:go (coord 0.05 0.05 'lt)
     @title{What is tidy data?}
     #:go (coord 0.5 0.2 'ct)
     (vl-append
      (current-line-sep)
      @ti{@tit{Idea:} data-frames are just a record of vectors...}
      @ti{...and we work with data by-variable...}
      @ti{...so you should be able to get a variable by selecting from the record.})
     #:go (coord 0.05 0.66 'lc)
     (vl-append
      (current-line-sep)
      @ti{@tit{To accomplish this:}}
      (pict:show @ti{Every column is exactly one variable.}
                 (at/after one))
      (pict:show @ti{Every row is exactly one observation.}
                 (at/after two))
      (pict:show @ti{Every cell is exactly one value.}
                 (at/after three)))
     #:go (coord 0.95 0.66 'rc)
     (vc-append
      (scale-to-fit
       (pict-case stage-name #:combine cc-superimpose
                  [(none) (blank)]
                  [(one) (bitmap "tidy-0-0.png")]
                  [(two) (bitmap "tidy-1-0.png")]
                  [(three) (bitmap "tidy-2-0.png")])
       310 300)
      (pict:show @cred{Credit: R for Data Science, chapter 12}
                 (after none))))))

(define (gss-pipeline-slides)
  (df-show-slide gss-sm "gss-sm" "gss_sm.csv"
                 '("bigregion" "religion" "kids" "ageq" "sex" "marital"))

  (define the-arrow (cc-superimpose
                     (arrowhead 56 0)
                     (colorize (arrowhead 50 0) "pale green")))
  (with-text-style
    #:defaults [#:face *global-font*]
    ([df-title #:size 25 #:bold? #t])

    (pslide/staged
     [step-1 step-2 step-3]
     #:go (tile 3 1)
     (vc-append
      @df-title{
        Individual-level data
        on region and religion
      }
      (show-pict gss-sm '("id" "bigregion" "religion")
                 #:font-size 15 #:no-header? #t))
     (pict:show
      (vc-append
       @df-title{
         Summary count by region of
         religious preferences
       }
       (show-pict (~> gss-sm
                      (group-with "bigregion" "religion")
                      (aggregate [count (bigregion) (vector-length bigregion)])
                      ungroup)
                  '("bigregion" "religion" "count")
                  #:font-size 15 #:no-header? #t))
      (at/after step-2))
     (pict:show
      (vc-append
       @df-title{
          Percent religious preference
          by census region
       }
       (show-pict (~> gss-sm
                      (group-with "bigregion" "religion")
                      (aggregate [count (bigregion) (vector-length bigregion)])
                      (create [freq ([count : vector]) (v/ count (sum count))]
                              [pct (freq) (round (* freq 100))])
                      ungroup)
                  '("bigregion" "religion" "pct")
                  #:font-size 15 #:no-header? #t))
      (at/after step-3))
     #:go (coord 0.325 0.5 'cc)
     (pict:show the-arrow (at/after step-2))
     #:go (coord 0.675 0.5 'cc)
     (pict:show the-arrow (at/after step-3)))))

(define (gss-example-slides)
  (define (make-gss-example level)
    (code
     (~> gss-sm
         (group-with "bigregion" "religion")
         #,(pict:show (code (aggregate [count (bigregion) (vector-length bigregion)]))
                      (>= level 2))
         #,(pict:show (code (create [frequency ([count : vector]) (v/ count (sum count))]
                                    #,(pict:show (code [percent (frequency) (round (* frequency 100))])
                                                 (>= level 4))))
                      (>= level 3))
         ungroup
         show)))

  (slide/staged
   [initial agg create-1 create-2]
   (vc-append
    50
    (make-gss-example stage)
    (pict-case stage-name #:combine lc-superimpose
               [(initial) (show-pict (~> gss-sm
                                         (group-with "bigregion" "religion")
                                         ungroup)
                                     '("id" "bigregion" "religion"))]
               [(agg) (show-pict (~> gss-sm
                                     (group-with "bigregion" "religion")
                                     (aggregate [count (bigregion) (vector-length bigregion)])
                                     ungroup)
                                 '("bigregion" "religion" "count"))]
               [(create-1) (show-pict (~> gss-sm
                                          (group-with "bigregion" "religion")
                                          (aggregate [count (bigregion) (vector-length bigregion)])
                                          (create [frequency ([count : vector]) (v/ count (sum count))])
                                          ungroup)
                                      '("bigregion" "religion" "count" "frequency"))]
               [(create-2) (show-pict (~> gss-sm
                                          (group-with "bigregion" "religion")
                                          (aggregate [count (bigregion) (vector-length bigregion)])
                                          (create [frequency ([count : vector]) (v/ count (sum count))]
                                                  [percent (frequency) (round (* frequency 100))])
                                          ungroup)
                                      '("bigregion" "religion" "count" "frequency" "percent"))])))

  (define religion-by-region
    (~> gss-sm
        (group-with "bigregion" "religion")
        (aggregate [count (bigregion) (vector-length bigregion)])
        (create [frequency ([count : vector]) (v/ count (sum count))]
                [percent (frequency) (round (* frequency 100))])
        ungroup))

  (define the-graph
    (graph #:data religion-by-region
           #:mapping (aes #:x "religion"
                          #:y "percent"
                          #:facet "bigregion")
           #:width 600 #:height 600
           (col)))

  (pslide
   #:go (coord 0.05 0.05 'lt)
   (code (graph #:data religion-by-region
                #:mapping (aes #:x "religion" #:y "percent" #:facet "bigregion")
                #:width 600 #:height 600
                (col)))
   #:go (coord 0.95 0.95 'rb)
   the-graph))

(define (billboard-example-slides)
  (df-show-slide billboard "billboard" "billboard.csv"
                 '("track" "artist" "date.entered" "wk1" "wk2" "wk3"))

  (define (make-billboard-example pivot create)
    (code (~> billboard
              #,pivot
              #,create
              show)))

  (pslide/staged
   [initial pivoted created]
   #:go (coord 0.5 0.05 'ct)
   (make-billboard-example
    (pict:show (code (pivot-longer (starting-with "wk")
                                   #:names-to "week"
                                   #:values-to "ranking"))
               (at/after pivoted))
    (pict:show (code (create [week (week) (string->number (substring week 2))]))
               (at/after created)))
   #:go (coord 0.5 0.95 'cb)
   (pict-case stage-name #:combine lc-superimpose
              [(initial) (show-pict billboard '("track" "artist" "date.entered" "wk1" "wk2" "wk3"))]
              [(pivoted) (show-pict (~> billboard
                                        (pivot-longer (starting-with "wk")
                                                      #:names-to "week"
                                                      #:values-to "ranking")
                                        (reorder "week")) ; for display purposes, avoiding #fs
                                    '("track" "artist" "date.entered" "week" "ranking"))]
              [(created) (show-pict (~> billboard
                                        (pivot-longer (starting-with "wk")
                                                      #:names-to "week"
                                                      #:values-to "ranking")
                                        (reorder "week")
                                        (create [week (week) (string->number (substring week 2))]))
                                    '("track" "artist" "date.entered" "week" "ranking"))]))

  (define tidy-billboard
    (~> billboard
        (pivot-longer (starting-with "wk")
                      #:names-to "week"
                      #:values-to "ranking")
        (create [week (week) (string->number (substring week 2))])))

  (define (make-billboard-graph-example where drop-na sorting ending)
    (code (~> tidy-billboard
              #,where
              #,drop-na
              #,sorting
              #,ending)))

  (pslide/staged
   [initial filtered dropped graphed]
   #:go (coord 0.5 0.05 'ct)
   (pict-case stage-name #:combine lt-superimpose
              [(initial filtered dropped)
               (make-billboard-graph-example
                (pict:show (code (where* (artist) ("Blink-182")))
                           (at/after filtered))
                (pict:show (code (drop-na "ranking"))
                           (at/after dropped))
                (pict:show (code (reorder "week"))
                           (at/after dropped))
                (code show))]
              [(graphed)
               (make-billboard-graph-example
                (code (where* (artist) ("Blink-182")))
                (code (drop-na "ranking"))
                (code (reorder "week"))
                (code (graph #:data _
                             #:mapping (aes #:x "week" #:y "ranking")
                             (lines))))])
   #:go (coord 0.5 0.95 'cb)
   (pict-case stage-name #:combine cc-superimpose
              [(initial) (show-pict tidy-billboard)]
              [(filtered) (show-pict (~> tidy-billboard
                                         (where* (artist) ("Blink-182"))))]
              [(dropped) (show-pict (~> tidy-billboard
                                        (where* (artist) ("Blink-182"))
                                        (drop-na "ranking")
                                        (reorder "week")))]
              [(graphed) (~> tidy-billboard
                             (where* (artist) ("Blink-182"))
                             (drop-na "ranking")
                             (reorder "week")
                             (graph #:data _
                                    #:mapping (aes #:x "week" #:y "ranking")
                                    (lines)))])))

(define (basic-operators-slides)
  (define example-df
    (row-df [grp trt adult juv]
            "a"  "b" 1     10
            "a"  "b" 2     20
            "b"  "a" 3     30
            "b"  "b" 4     40
            "b"  "b" 5     50))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 25 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])

    (pslide/staged
     [create-s slice-s where-s aggregate-s]
     #:go (coord 0.05 0.05 'lt)
     @title{Basic wrangling operators}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      (pict:show
       @ti{@tt{create}: adds new variables, dependent on existing variables}
       (at/after create-s))
      (pict:show
       @ti{@tt{slice}: picks variables to retain based on their names}
       (at/after slice-s))
      (pict:show
       @ti{@tt{where}: picks variables to retain based on their values}
       (at/after where-s))
      (pict:show
       @ti{@tt{aggregate}: reduces many values down to a summary}
       (at/after aggregate-s)))
     #:go (coord 0.15 0.45 'ct)
     (tag-pict (show-pict example-df #:no-header? #t) 'orig-df)
     #:go (coord 0.85 0.45 'ct)
     (tag-pict
      (pict-case stage-name
                 [(create-s) (show-pict (create example-df [total (adult juv) (+ adult juv)]) #:no-header? #t)]
                 [(slice-s) (show-pict (slice example-df (containing "t")) #:no-header? #t)]
                 [(where-s) (show-pict (where example-df (adult) (> adult 3)) #:no-header? #t)]
                 [(aggregate-s) (show-pict (aggregate example-df [sum (adult) (vector-length adult)]) #:no-header? #t)])
      'wrangled-df)
     #:set
     (let ([p ppict-do-state])
       (pin-arrow-line
        20 p
        (find-tag p 'orig-df) rc-find
        (find-tag p 'wrangled-df) lc-find
        #:label (typeset-code
                 (case stage-name
                   [(create-s) #'(create [total (adult juv) (+ adult juv)])]
                   [(slice-s) #'(slice (containing "t"))]
                   [(where-s) #'(where (adult) (> adult 3))]
                   [(aggregate-s) #'(aggregate [sum (adult) (vector-length adult)])])))))

    (define (sum vec)
      (for/sum ([v (in-vector vec)] #:when v) v))

    (pslide/staged
     [no-group one-group two-group]
     #:go (coord 0.05 0.05 'lt)
     @title{Grouping, relating to operators}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      @ti{@tit{Idea:} a lot of data manipulation tasks happen "in groups"}
      @ti{So, we can compose existing operators with grouping to avoid repeating ourselves})
     #:go (coord 0.15 0.45 'ct)
     (tag-pict (show-pict example-df #:no-header? #t) 'orig-df)
     #:go (coord 0.85 0.45 'ct)
     (tag-pict
      (pict-case stage-name
                 [(no-group) (show-pict (~> example-df
                                            (aggregate {adult-sum (adult) (sum adult)}
                                                       [juv-sum (juv) (sum juv)]))
                                        '("adult-sum" "juv-sum")
                                        #:no-header? #t)]
                 [(one-group) (show-pict (~> example-df
                                             (group-with "grp")
                                             (aggregate [adult-sum (adult) (sum adult)]
                                                        [juv-sum (juv) (sum juv)])
                                             ungroup)
                                         '("adult-sum" "juv-sum" "grp")
                                         #:no-header? #t)]
                 [(two-group) (show-pict (~> example-df
                                             (group-with "grp" "trt")
                                             (aggregate [adult-sum (adult) (sum adult)]
                                                        [juv-sum (juv) (sum juv)])
                                             ungroup)
                                         '("adult-sum" "juv-sum" "grp" "trt")
                                         #:no-header? #t)]
                 ;; hack for alignment
                 [(useless) (show-pict example-df #:no-header? #t)])
      'wrangled-df)
     #:set
     (let ([p ppict-do-state])
       (pin-arrow-line
        20 p
        (find-tag p 'orig-df) rc-find
        (find-tag p 'wrangled-df) lc-find
        #:label
        (typeset-code
         (case stage-name
           [(no-group)
            #'(~> example-df
                  code:blank
                  (aggregate [adult-sum (adult) (sum adult)]
                             [juv-sum (juv) (sum juv)]))]
           [(one-group)
            #'(~> example-df
                  (group-with "grp")
                  (aggregate [adult-sum (adult) (sum adult)]
                             [juv-sum (juv) (sum juv)]))]
           [(two-group)
            #'(~> example-df
                  (group-with "grp" "trt")
                  (aggregate [adult-sum (adult) (sum adult)]
                             [juv-sum (juv) (sum juv)]))])))))))

(define (tidying-operators-slides)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 25 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])

    (pslide/staged
     [pivot-s unnest-s separate-s sort-s]
     #:go (coord 0.05 0.05 'lt)
     @title{Tidying operators}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      (pict:show
       @ti{@tt{pivot}: changes the shape of the data to be longer or wider}
       (at/after pivot-s))
      (pict:show
       @ti{@tt{unnest}: spreads nested structure (like lists) into multiple variables}
       (at/after unnest-s))
      (pict:show
       @ti{@tt{separate}: spreads strings into multiple variables}
       (at/after separate-s))
      (pict:show
       @ti{@tt{reorder}: sorts the data according to some variable/comparator}
       (at/after sort-s))))))

(define (implementation-details-slides)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 25 #:face *mono-font*]
     [tts #:size 15 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Under the hood: macros, macros everywhere}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      @ti{Most of Sawzall's individual operators are macros}
      @ti{All operations are either @tt{data-frame? -> data-frame?}, or a wrapper structure}
      @ti{The consistency of operations without side-effects means that operations compose with @tt{~>}}
      @ti{Racket is really good at writing down what you want to write, and figuring it out later})
     #:go (coord 0.13 0.6 'lt)          ; SCREAMS
     (code
      (~> #,(tag-pict (code by-word) 'init)
          #,(tag-pict (code (group-with "word")) 'grouping)
          #,(tag-pict (code (aggregate [count-sum (count) (sum count)])) 'aggregating)
          (reorder (cons "count-sum" >))
          (take-rows 0 20)))
     #:set
     (let ([p ppict-do-state])
       (pin-arrow-line
        20 p
        (find-tag p 'init) rc-find
        (find-tag p 'grouping) rc-find
        #:start-angle 0
        #:end-angle pi
        #:start-pull 1.5
        #:label @tts{data-frame? -> grouped-data-frame?}
        #:x-adjust-label 615
        #:y-adjust-label (/ 25 2)))
     #:set
     (let ([p ppict-do-state])
       (pin-arrow-line
        20 p
        (find-tag p 'grouping) rc-find
        (find-tag p 'aggregating) rc-find
        #:start-angle 0
        #:end-angle pi
        #:start-pull 1.5
        #:label @tts{grouped-data-frame? -> data-frame?}
        #:x-adjust-label 400
        #:y-adjust-label (/ 25 2))))
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Under the hood: syntax class DSLs}
     (vl-append
      (current-line-sep)
      @ti{Racket preaches language-oriented programming, but what if you want languages @tit{inside} @tt{#lang racket}?}
      @ti{@tit{Syntax classes} (from @tt{syntax/parse}) let you parse embedded DSLs at compile-time}
      @ti{Sawzall uses these extensively for various operators which speak their own language (namely @tt{slice})}))
    (pslide (t "TODO: example"))))

(define (uses-directions-slides)
  (define not-cancelled
    (~> flights
        (where (arr_delay dep_delay) (and arr_delay dep_delay))))
  (define delays-by-tailnum
    (~> not-cancelled
        (group-with "tailnum")
        (aggregate [delay (arr_delay) (mean arr_delay)]
                   [N (arr_delay) (vector-length arr_delay)])
        (where (N) (> N 25))))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titleit #:size 50 #:bold? #t #:italic? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 25]
     [tt #:size 25 #:face *mono-font*]
     [tit #:size 25 #:italic? #t]
     [ti #:size 25
         #:transform (λ (p) (t #:h-append hc-append #:left-pad 30 "• " p))]
     [cred #:size 10])
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What's Sawzall already good for?}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      @ti{Processing small, in-memory datasets
          already works very well}
      @ti{Basic data science tasks can be completed,
          including most of Hadley Wickham's book
          @tit{R for Data Science}}
      @ti{There's a whole other library for visualization
          (and a Scheme workshop talk about it)})
     #:go (coord 0.95 0.95 'rb)
     (graph #:data delays-by-tailnum
            #:mapping (aes #:x "N" #:y "delay")
            #:title "Arrival delay by tail number, NYC 2013"
            #:x-label "Tail number"
            #:y-label "Delay on arrival (minutes)"
            #:x-min -2 #:height 550 #:width 550
            (points #:alpha 1/10 #:color "black")))
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Future directions}
     (vl-append
      (current-line-sep)
      @ti{Feature parity with R/tidyverse is a non-goal}
      @ti{Performance still leaves a lot to be desired}
      @ti{Currently dependent on Alex Harsanyi's @tt{data-frame} library, though it could be abstracted away from it}
      (hc-append (ghost (filled-rectangle 50 25))
                 @ti{This could be a generic interface, possibly interfacing with real databases})))))

(define (bunch-of-plots-slide)
  (define (scale-to-4-panel p)
    (scale-to-fit p (/ (get-client-w) 2) (/ (get-client-h) 2)))

  (define oecd-plot
    (scale-to-4-panel
     (graph #:data oecd
            #:mapping (aes #:x "year" #:y "diff")
            #:title "Difference between US and OECD average life expectancies"
            #:x-label "Year" #:y-label "Difference (years)"
            #:y-min -2 #:y-max 2
            #:width 600 #:height 400
            #:legend-anchor 'no-legend
            (col #:mapping (aes #:discrete-color "hi_lo")))))

  (define anscombe-facetable
    (~> anscombe
        (create [nrow ([x1 : vector]) (build-vector (vector-length x1) (λ (x) x))])
        (pivot-longer (not "nrow") #:names-to "name" #:values-to "val")
        (separate "name" #:into '("x-or-y" "quadrant") #:separator 1)
        (pivot-wider #:names-from "x-or-y" #:values-from "val")
        (slice (not "nrow"))))
  (define anscombe-plot
    (scale-to-4-panel
     (graph #:data anscombe-facetable
            #:mapping (aes #:x "x" #:y "y" #:facet "quadrant")
            #:width 600 #:height 470    ; weh
            #:title "Anscombe's Quartet"
            (points)
            (fit #:width 3))))

  (define another-gss-plot
    (scale-to-4-panel
     (graph #:data gss-sm
            #:title "Religious preferences among regions, GSS 2016"
            #:mapping (aes #:x "bigregion" #:group "religion")
            #:width 600 #:height 400
            (stacked-bar #:mode 'prop))))

  (define sorted-countries
    (~> organdata
        (group-with "country")
        (aggregate [med (donors) (median < (vector-filter identity donors))])
        (reorder "med")
        (df-select "country")))
  (define organdata-sorted
    (reorder organdata (cons "country" (by-vector sorted-countries))))
  (define organdata-plot
    (scale-to-4-panel
     (graph #:data organdata-sorted
            #:title "Organ donation count by country, over time"
            #:mapping (aes #:x "donors" #:y "country")
            #:width 600 #:height 400
            (boxplot #:invert? #t))))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([t #:size 30]
     [lnk #:size 30 #:color "blue"])
    (pslide/staged
     [no-box yes-box]
     #:go (tile 2 2)
     oecd-plot
     anscombe-plot
     another-gss-plot
     organdata-plot
     #:go (coord 0.5 0.5 'cc)
     (pict:show
      (shadow-frame
       (vc-append
        (current-line-sep)
        (hyperlinkize @lnk{https://github.com/ralsei/sawzall})
        (hyperlinkize @lnk{https://docs.racket-lang.org/graphite-tutorial})
        (hc-append @t{Thanks to: }
                   (vc-append
                    (current-line-sep)
                    (hyperlinkize @lnk{https://socviz.co})
                    (hyperlinkize @lnk{https://r4ds.had.co.nz})))))
      (at yes-box)))))

;;;; main
(module+ main
  ;; (title-slide)
  ;; (gss-pipeline-slides)
  ;; (gss-example-slides)
  ;; (sawzall-intro-slides)
  ;; (basic-operators-slides)
  ;; (billboard-example-slides)
  ;; (tidying-operators-slides)
  (implementation-details-slides)
  (uses-directions-slides)
  (bunch-of-plots-slide)
  )
