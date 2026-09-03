# Create Classification Bar Plot

Creates a horizontal bar plot showing classification distance scores.
Because these indices are distance metrics, **shorter bars** represent
**better matches**. The plot automatically sorts the best matches to the
top.

## Usage

``` r
plot_classification_bar(
  object,
  indextype = "cod",
  types = 20,
  order = c("value", "alphabetical", "given"),
  show_values = FALSE,
  fill_color = "steelblue",
  ...
)
```

## Arguments

- object:

  An object of class `inbovegclassification` containing the results.

- indextype:

  Character string indicating the name of the index column to use.
  Default "cod".

- types:

  Either a number indicating how many top types to show, or a character
  vector of syntaxon codes to display.

- order:

  Character. How to order the classes. One of:

  - `"value"`: (Default) Best match (lowest value) is at the top.

  - `"alphabetical"`: Alphabetical by syntaxon code (A at top).

  - `"given"`: Order provided by the `types` vector.

- show_values:

  Logical. If TRUE, adds text labels with the exact values at the end of
  the bars. Default FALSE.

- fill_color:

  Character. Color of the bars. Default "steelblue".

- ...:

  Additional arguments (not used).

## Value

A ggplot object
