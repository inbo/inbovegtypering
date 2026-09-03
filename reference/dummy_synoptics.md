# Dummy Synoptic Table

A synthetic synoptic table defining 10 vegetation syntaxa. It provides
the frequency and characteristic cover for 20 dummy species within these
types. Syntaxa "SYN-01" and "SYN-02" are designed to be similar to
recordings "REC-01" and "REC-02".

## Usage

``` r
data(dummy_synoptics)
```

## Format

A data frame with 200 rows and 4 variables:

- syntaxon_code:

  Character. Unique identifier for the vegetation type (syntaxon) (e.g.,
  "SYN-01").

- species_number:

  Integer. Internal numeric identifier for the species (1-20).

- frequency:

  Numeric. The frequency of the species within the syntaxon (percentage
  0-100).

- mean_if_present:

  Numeric. The mean cover of the species when it is present (percentage
  0-100).
