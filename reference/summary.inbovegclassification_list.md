# Summary for a List of Vegetation Classifications

Summarizes results for multiple recordings. Allows filtering for
specific recording IDs and passing sorting parameters to the individual
summaries.

## Usage

``` r
# S3 method for class 'inbovegclassification_list'
summary(object, recording_ids = NULL, ...)
```

## Arguments

- object:

  An object of class `inbovegclassification_list`.

- recording_ids:

  Character vector. Specific recording IDs to summarize. If NULL
  (default), all recordings in the list are summarized.

- ...:

  Arguments passed to `summary.inbovegclassification` (e.g., `sort_by`,
  `indices`, `top_n`, `digits`).

## Value

A list of summaries (class `summary.inbovegclassification_list`).
