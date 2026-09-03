# Dummy Vegetation Recordings

A synthetic dataset containing vegetation recordings (relevés) generated
for testing classification functions. It includes 5 recordings with
varying species composition. Recordings "REC-01" and "REC-02" are
designed to be very similar.

## Usage

``` r
data(dummy_recordings)
```

## Format

A data frame with 50 rows and 4 variables:

- recording:

  Character. Unique identifier for the recording event (relevé) (e.g.,
  "REC-01").

- species_number:

  Integer. Internal numeric identifier for the species (1-20).

- pct_value:

  Numeric. The cover percentage of the species (0-100).

- layer_code:

  Character. Code indicating the vegetation layer (fixed as "K" for herb
  layer).
