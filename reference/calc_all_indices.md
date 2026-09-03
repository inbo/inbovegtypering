# Core Calculation of ASSOCIA Indices

Computes Weirdness, Incompleteness, Likelihood, MED, and CoD based on
van Tongeren et al. (2008). This version implements normalization to
remove richness bias and uses the corrected MED formula from the manual.

## Usage

``` r
calc_all_indices(data, r, s, t)
```

## Arguments

- data:

  A data frame containing:

  - `presence`: Logical, whether species is present in the relevé.

  - `pct_presence`: Frequency of species in the syntaxon (0-1).

  - `pct_value`: Observed abundance in the relevé.

  - `cover_if_present`: Characteristic abundance (CA) of the species in
    the syntaxon.

- r:

  Weight for present species in the qualitative index (typically
  0.5-1.0).

- s:

  Power exponent (0-1) balancing Qualitative (s) vs Quantitative (1-s)
  components.

- t:

  Weight for present species in MED (typically 0.5-1.0).

## Value

A data frame with raw and normalized indices. Normalized values of 0
indicate an "average" match, while values \> 1 indicate atypical
relevés.
