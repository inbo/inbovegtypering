# Plot Method for inbovegclassification_list Objects

Plots classification roses for multiple recordings, arranged in a grid.
Includes safeguards against printing too many pages.

## Usage

``` r
# S3 method for class 'inbovegclassification_list'
plot(x, ncol = 2, nrow = 2, max_pages = 10, force = FALSE, ...)
```

## Arguments

- x:

  An object of class "inbovegclassification_list".

- ncol:

  Integer. Number of columns in the plot grid. Default 2.

- nrow:

  Integer. Number of rows in the plot grid. Default 2.

- max_pages:

  Integer. Maximum number of pages allowed to print. Default 10.

- force:

  Logical. If TRUE, overrides the max_pages limit. Default FALSE.

- ...:

  arguments passed to `plot.inbovegclassification`.

## Value

Prints plots to the active device and returns `invisible(NULL)`.
