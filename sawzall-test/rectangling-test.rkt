#lang racket
(require rackunit
         sawzall
         "util.rkt")

(define deep-df
  (column-df
   [character #("Toothless" "Dory" "Holly")]
   [metadata (vector (hash 'species "dragon"
                           'color   "black"
                           'films   (vector "How to Train Your Dragon"
                                            "How to Train Your Dragon 2"
                                            "How to Train Your Dragon: The Hidden World"))
                     (hash 'species "blue tang"
                           'color   "blue"
                           'films   (vector "Finding Nemo"
                                            "Finding Dory"))
                     (hash 'species "glaceon"
                           'color   "also blue"
                           'films   #f))]))

(define unnest-1 (unnest-wider deep-df "metadata"))
(define unnest-1-result
  (row-df [character  species     color       films]
          "Toothless" "dragon"    "black"     (vector "How to Train Your Dragon"
                                                      "How to Train Your Dragon 2"
                                                      "How to Train Your Dragon: The Hidden World")
          "Dory"      "blue tang" "blue"      (vector "Finding Nemo"
                                                      "Finding Dory")
          "Holly"    "glaceon"   "also blue"  #f))

(define unnest-2 (unnest-longer unnest-1-result "films"))
(define unnest-2-result
  (row-df [character  species     color       films]
          "Toothless" "dragon"    "black"     "How to Train Your Dragon"
          "Toothless" "dragon"    "black"     "How to Train Your Dragon 2"
          "Toothless" "dragon"    "black"     "How to Train Your Dragon: The Hidden World"
          "Dory"      "blue tang" "blue"      "Finding Nemo"
          "Dory"      "blue tang" "blue"      "Finding Dory"
          "Holly"     "glaceon"   "also blue" #f))

(define unnest-3 (unnest-longer deep-df "metadata"))
(define unnest-3-result
  (row-df [character  metadata-keys metadata]
          "Toothless" 'color        "black"
          "Toothless" 'species      "dragon"
          "Toothless" 'films        (vector "How to Train Your Dragon"
                                            "How to Train Your Dragon 2"
                                            "How to Train Your Dragon: The Hidden World")
          "Dory"      'color        "blue"
          "Dory"      'species      "blue tang"
          "Dory"      'films        (vector "Finding Nemo"
                                            "Finding Dory")
          "Holly"     'color        "also blue"
          "Holly"     'species      "glaceon"
          "Holly"     'films        #f))

(define unnest-4 (unnest-wider unnest-1-result "films"))
(define unnest-4-result
  (row-df [character  species     color       idx-1 idx-2 idx-3]
          "Toothless" "dragon"    "black"     "How to Train Your Dragon"
                                              "How to Train Your Dragon 2"
                                              "How to Train Your Dragon: The Hidden World"
          "Dory"      "blue tang" "blue"      "Finding Nemo"
                                              "Finding Dory"
                                              #f
          "Holly"     "glaceon"   "also blue" #f #f #f))

(module+ test
  (check data-frame~=? unnest-1 unnest-1-result)
  (check data-frame~=? unnest-2 unnest-2-result)
  (check data-frame~=? unnest-3 unnest-3-result)
  (check data-frame~=? unnest-4 unnest-4-result))
