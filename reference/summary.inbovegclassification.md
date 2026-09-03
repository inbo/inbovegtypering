# Summary for a Single Vegetation Classification

Summarizes the classification results for a single recording. Allows
sorting by specific indices and selecting specific columns to display.

## Usage

``` r
# S3 method for class 'inbovegclassification'
summary(
  object,
  sort_by = "cod",
  indices = c("cod", "nrm_cod", "med", "nrm_llk", "nrm_wrd", "nrm_inc"),
  top_n = 20,
  digits = 3,
  ...
)
```

## Arguments

- object:

  An object of class `inbovegclassification`.

- sort_by:

  Character string. The column name to sort by (ascending). Defaults to
  "cod".

- indices:

  Character vector. The indices (columns) to display in the output.
  Defaults to c("cod", "nrm_cod", "med", "nrm_llk", "nrm_wrd",
  "nrm_inc"). Available options are the columns present in
  `object$results`, including normalized indices such as "nrm_cod",
  "nrm_llk", "nrm_wrd", and "nrm_inc".

- top_n:

  Integer. The number of best matching syntaxa to return. Defaults
  to 20. Set to Inf to return all.

- digits:

  Number of digits to display per index, defaults to 3

- ...:

  Additional arguments (not used).

## Value

A data frame (invisibly) containing the sorted and filtered results,
with additional class `summary.inbovegclassification` for pretty
printing.
