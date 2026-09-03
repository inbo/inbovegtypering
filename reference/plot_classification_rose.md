# Create Classification Rose Plot

Creates a star-shaped radial plot ("classification rose") showing
classification results. The scaling is relative to the best match in the
set.

## Usage

``` r
plot_classification_rose(
  object,
  indextype = "cod",
  types = 12,
  relative_scale = TRUE,
  sensitivity = 1,
  min_length = 0.2,
  order = c("value", "alphabetical", "given"),
  ...
)
```

## Arguments

- object:

  An object of class `inbovegclassification` containing the results.

- indextype:

  Character string indicating the name of the index column to use.
  Default "cod" (Lower values = better match).

- types:

  Either a number indicating how many top types to show, or a character
  vector of syntaxon codes to display.

- relative_scale:

  Logical. If TRUE (default), the spoke length is calculated relative to
  the best match's score. If FALSE, it normalizes min-max within the
  subset.

- sensitivity:

  Numeric. Controls how quickly spoke length decays for worse matches
  when `relative_scale = TRUE`. Default 1.0 (linear ratio). Higher
  values (\>1) punish bad matches more visible; values \< 1 make
  differences subtler.

- min_length:

  Minimum length for normalization (default 0.1) to ensure visibility.

- order:

  Character. How to order the classes. One of "value" (default),
  "alphabetical", or "given".

- ...:

  Additional arguments (not used).

## Value

A ggplot object
