# Bar Plot Method for inbovegclassification Objects

Creates a horizontal bar plot for a single vegetation classification
object. Dispatches to
[`plot_classification_bar()`](https://inbo.github.io/inbovegtypering/reference/plot_classification_bar.md).

## Usage

``` r
# S3 method for class 'inbovegclassification'
barplot(height, indextype = "cod", types = 20, ...)
```

## Arguments

- height:

  An object of class "inbovegclassification".

- indextype:

  Character string. The index to plot (e.g., "cod", "med"). Defaults to
  "cod".

- types:

  Number of top matches to show (default 20) or specific codes.

- ...:

  Additional arguments passed to `plot_classification_bar`.

## Value

A ggplot object.
