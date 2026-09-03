# Plot Method for inbovegclassification Objects

Generic plot method that dispatches to
[`plot_classification_rose()`](https://inbo.github.io/inbovegtypering/reference/plot_classification_rose.md).
Visualizes the best matching syntaxa for a vegetation recording.

## Usage

``` r
# S3 method for class 'inbovegclassification'
plot(x, indextype = "cod", types = 12, plot_type = "rose", ...)
```

## Arguments

- x:

  An object of class "inbovegclassification"

- indextype:

  Character string. The index to plot (e.g., "cod", "med",
  "likelihood"). Defaults to "cod".

- types:

  Either a number indicating how many top types to show (default 12), or
  a character vector of syntaxon codes to display.

- plot_type:

  type of plot "rose" or "bar"

- ...:

  Additional arguments passed to
  [`plot_classification_rose()`](https://inbo.github.io/inbovegtypering/reference/plot_classification_rose.md)

## Value

A ggplot object
