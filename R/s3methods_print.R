#' Print Method for inbovegclassification Objects
#'
#' @description
#' Prints the results of vegetation classification, showing the top matches.
#'
#' @param x An object of class "inbovegclassification"
#' @param n Number of top matches to show (default 10)
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
#' @importFrom utils head
print.inbovegclassification <- function(x, n = 10, ...) {
  indextype <- attr(x, "indextype")
  recording <- attr(x, "RecordingGivid")

  if (is.null(indextype)) indextype <- "Unknown"
  if (is.null(recording)) recording <- "Unknown"

  cat(sprintf("--- Classification for Relevé: %s ---\n", recording))
  cat(sprintf("Method: %s (Lower values are better matches)\n\n", indextype))

  # Get the main index column name (the one *after* RecordingGivid and syntaxonCode)
  # This is safer than assuming the column name matches indextype
  if (ncol(x) > 2) {
    index_col_name <- names(x)[3]
    # Re-order by this column just in case
    print_data <- x[order(x[[index_col_name]]), ]

    # Format for printing
    print_data_format <- data.frame(
      Rank = 1:nrow(print_data),
      Syntaxon = print_data$syntaxonCode,
      Value = format(print_data[[index_col_name]], digits = 4, nsmall = 2),
      stringsAsFactors = FALSE
    )
    names(print_data_format)[3] <- indextype

    print(utils::head(print_data_format, n = n), row.names = FALSE)
  } else {
    cat("No classification data available.\n")
  }

  cat(sprintf(
    "\n... showing top %d of %d syntaxa.\n",
    min(n, nrow(x)), nrow(x)
  ))

  invisible(x)
}

#' Print method for inbovegclassification_list Objects
#'
#' @description
#' Prints the classification results for each relevé in the list.
#'
#' @param x list of classifications
#' @param ... arguments passed to print.inbovegclassification
#'
#' @return Invisibly returns the input object
#' @export
print.inbovegclassification_list <- function(x, ...) {
  cat(sprintf("=== INBOVEGTYPERING Classification List ===\n"))
  cat(sprintf(
    "Contains classifications for %d relevé(s):\n%s\n",
    length(x), paste(names(x), collapse = ", ")
  ))
  cat("==============================================\n\n")

  # Iterate and print each element
  invisible(lapply(x, function(element) {
    print(element, ...)
    cat("\n") # Add a separator
  }))
}

#' Print Method for summary.inbovegclassification Objects
#'
#' @param x A summary.inbovegclassification object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#' @export
print.summary.inbovegclassification <- function(x, ...) {
  cat(sprintf("--- Summary for Relevé: %s ---\n", x$RecordingGivid))
  cat(sprintf("Method: %s\n", x$indextype))
  cat(sprintf("Total syntaxa compared: %d\n\n", x$n_total_types))

  cat("Best Matching Syntaxon:\n")
  cat(sprintf("  %s (Value: %.4f)\n\n", x$best_match, x$best_value))

  cat("Top Matches:\n")
  print(x$top_matches, row.names = FALSE)

  cat("\nValue Distribution Statistics:\n")
  print(x$value_stats)

  invisible(x)
}
