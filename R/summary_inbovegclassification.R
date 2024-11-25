#' Summary Method for inbovegclassification Objects
#'
#' @description
#' Provides a summary of vegetation classification results,
#' including best matches and additional statistics.
#'
#' @param object An object of class "inbovegclassification"
#' @param n_types Number of types to show (default 10)
#' @param types Optional character vector of specific syntaxon codes to show
#' @param order Ordering of results:
#' "likelihood" (default), "alphabetical", or "given"
#' @param ... Additional arguments passed to methods
#' @importFrom stats quantile
#'
#' @return A list with summary statistics
#' @export
summary.inbovegclassification <-
  function(object,
           n_types = 10,
           types = NULL,
           order = c("likelihood", "alphabetical", "given"), ...) {
  order <- match.arg(order)
  indextype <- attr(object, "indextype")

  # Filter and order data similar to print method
  if (is.null(types)) {
    results <- object |>
      slice_head(n = n_types)
  } else {
    results <- object |>
      filter(.data$syntaxoncode %in% types)

    results <- switch(order,
      "likelihood" = results |> arrange(.data[[indextype]]),
      "alphabetical" = results |> arrange(.data$syntaxoncode),
      "given" = results |>
        mutate(order = match(.data$syntaxoncode, types)) |>
        arrange(order) |>
        select(-order)
    )
  }

  # Calculate summary statistics
  stats <- list(
    indextype = indextype,
    n_total_types = nrow(object),
    best_match = results$syntaxoncode[1],
    best_value = results[[indextype]][1],
    selected_types = results,
    value_range = range(object[[indextype]]),
    value_quartiles = quantile(object[[indextype]])
  )

  # Create print method for the summary
  class(stats) <- "summary.inbovegclassification"

  return(stats)
}
