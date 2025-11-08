#' Print Method for summary.inbovegclassification Objects
#'
#' @param x A summary.inbovegclassification object
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns the input object
#' @export
print.summary.inbovegclassification <- function(x, ...) {
  cat("Vegetation Classification Summary\n")
  cat("================================\n\n")

  cat(sprintf("Index type: %s\n", x$indextype))
  cat(sprintf("Total number of types: %d\n", x$n_total_types))
  cat(sprintf(
    "Best matching type: %s (value: %.3f)\n",
    x$best_match, x$best_value
  ))

  cat("\nValue distribution:\n")
  cat(sprintf("  Range: %.3f to %.3f\n", x$value_range[1], x$value_range[2]))
  cat("  Quartiles:\n")
  print(x$value_quartiles)

  cat("\nSelected types:\n")
  print(as.data.frame(x$selected_types), row.names = FALSE)

  invisible(x)
}
