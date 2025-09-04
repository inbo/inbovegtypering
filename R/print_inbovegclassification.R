#' Print Method for inbovegclassification Objects
#'
#' @description
#' Prints the results of vegetation classification, showing the top matches.
#'
#' @param x An object of class "inbovegclassification"
#' @param n_types Number of types to show (default 10)
#' @param types Optional character vector of specific syntaxon codes to show
#' @param order Ordering of results: "likelihood" (default),
#' "alphabetical", or "given"
#' @param ... Additional arguments passed to methods
#'
#' @return Invisibly returns the input object
#' @export
print.inbovegclassification <-
  function(x,
           n_types = 10,
           types = NULL,
           order = c("likelihood", "alphabetical", "given"), ...) {
    order <- match.arg(order)
    indextype <- attr(x, "indextype")

    # Filter and order data
    if (is.null(types)) {
      results <- x |>
        slice_head(n = n_types)
    } else {
      results <- x |>
        filter(.data$syntaxonCode %in% types)

      results <- switch(order,
        "likelihood" = results |> arrange(.data[[indextype]]),
        "alphabetical" = results |> arrange(.data$syntaxonCode),
        "given" = results |>
          mutate(order = match(.data$syntaxonCode, types)) |>
          arrange(order) |>
          select(-order)
      )

      if (nrow(results) != length(types)) {
        missing_types <- setdiff(types, results$syntaxonCode)
        warning(sprintf(
          "Some requested types not found in data: %s",
          paste(missing_types, collapse = ", ")
        ))
      }
    }

    # Print header
    cat(sprintf("Vegetation Classification using %s index\n\n", indextype))

    # Print results
    print(as.data.frame(results), row.names = FALSE)
    # cat("\n",
    #     "indextype: ", attr(results, "indextype"), "\n",
    #     "recording: ", attr(results, "RecordingGivid"), "\n"
    # )

    invisible(x)
  }

######

#' Print classification
#'
#' @param x list of classifications
#' @param ... arguments passed to print.inbovegclassification
#'
#' @returns print output
#' @export
#'
print.inbovegclassification_list <- function(x, ...) {
  for (i in names(x)) {
    print.inbovegclassification(x[[i]], ...)
  }
}
