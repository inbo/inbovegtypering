# Classify Vegetation Recordings

Calculates similarity indices (Weirdness, Incompleteness, Likelihood,
MED, CoD) for vegetation recordings against a synoptic table of syntaxa.

## Usage

``` r
classify_vegetation(data, synoptics, layer = "ALL", r = 0.5, s = 0.6, t = 0.5)
```

## Arguments

- data:

  A data frame containing the vegetation recordings. Must contain
  columns: `recording` (ID), `species_number`, `pct_value` (cover
  percentage 0-100), and `layer_code`.

- synoptics:

  A data frame, a file path, or the string "default". If a data frame,
  it must contain columns: `syntaxon_code`, `species_number`,
  `frequency` (0-100), and `mean_if_present` (0-100). If "default" or
  "package", the function looks for a file named `synoptic_table.csv` in
  the package's `extdata` directory.

- layer:

  Character string indicating the layer code to filter from `data`.
  Defaults to "ALL", ( other examples "K" (Herb layer)).

- r:

  Numeric. Weighting parameter for the "Weirdness" component in the
  Composite Distance (CoD). Default is 0.50.

- s:

  Numeric. Weighting exponent for the qualitative part (Likelihood) in
  the CoD. Default is 0.60.

- t:

  Numeric. Weighting parameter for the presence/absence balance in the
  Modified Euclidean Distance (MED). Default is 0.50.

## Value

An object of class `inbovegclassification_list`, which is a list of
objects of class `inbovegclassification` (one per recording).
