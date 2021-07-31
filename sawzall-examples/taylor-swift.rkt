#lang racket
(require data-frame
         csv-reading
         graphite
         sawzall
         threading)

;; read in all the data
(define raw-lyrics (df-read/csv "data/taylor_swift_lyrics.csv"))

(define stop-words (apply set (first (call-with-input-file "data/stop-words.csv" csv->list))))

;; currently, we have each song line-by-line. we want *all* the lyrics
;; for each song.
(define full-lyrics
  (~> raw-lyrics
      ;; group in album and track title,
      ;; both because we actually want to group by track title
      ;; and to retain the album information after aggregating
      (group-with "album" "track_title")
      ;; append all of the strings with spaces
      ;; map ~a (format) to avoid parsing the lyric "22" in the song "22" as an integer
      (aggregate [lyrics (lyric) (string-join (vector->list (vector-map ~a lyric)) " ")])
      ungroup))

;; strips punctuation from a string
(define (strip-punctuation str)
  (string-normalize-spaces (regexp-replace* #px"[.,/#!$%\\^&\\*;:{}=\\-_`~()\"\"\\?]" str " ")))

(define (get-frequency lst)
  (define frequencies (make-hash))
  (for ([v (in-list lst)])
    (hash-update! frequencies v add1 0))
  frequencies)

(define by-word
  (~> full-lyrics
      ;; remove all punctuation (except ones that modify the meaning)
      ;; and also remove uppercase
      (create [lyrics (lyrics) (string-downcase (strip-punctuation lyrics))])
      ;; then turn them into hashes
      ;; this could be combined into the above operation, but I'm not for clarity
      (create [lyrics (lyrics) (get-frequency (string-split lyrics))])
      ;; then turn those hash tables into regular columnar variables
      (unnest-longer "lyrics"
                     #:keys-to "word"
                     #:values-to "count"
                     #:remove? #t)
      ;; remove "stop words" (really common english)
      (where (word) (not (set-member? stop-words word)))))

(define (sum vec) (for/sum ([v (in-vector vec)]) v))

(~> by-word
    (group-with "word")
    (aggregate [count-sum (count) (sum count)])
    (reorder (cons "count-sum" >))
    (take-rows 0 20)
    (graph #:data _
           #:mapping (aes #:x "word" #:y "count-sum")
           #:width 1000 #:height 600
           (col)))
