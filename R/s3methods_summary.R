#' Summary Method for inbovegclassification Objects
#'
#' @description
#' Provides a summary of vegetation classification results,
#' including best matches and additional statistics.
#'
#' @param object An object of class "inbovegclassification"
#' @param n Number of top matches to show (default 5)
#' @param ... Additional arguments (not used)
#'
#' @return A list object of class `summary.inbovegclassification`
#' @export
#' @importFrom stats quantile
#' @importFrom utils head
summary.inbovegclassification <- function(object, n = 5, ...) {
  indextype <- attr(object, "indextype")
  recording <- attr(object, "RecordingGivid")

  if (ncol(object) <= 2) {
    stop("Classification object is empty or malformed.")
  }

  # Get the main index column
  index_col_name <- names(object)[3]
  index_values <- object[[index_col_name]]

  # Prepare top matches
  top_matches_df <- utils::head(object, n)
  top_matches_format <- data.frame(
    Rank = 1:nrow(top_matches_df),
    Syntaxon = top_matches_df$syntaxonCode,
    Value = format(top_matches_df[[index_col_name]], digits = 4, nsmall = 2),
    stringsAsFactors = FALSE
  )
  names(top_matches_format)[3] <- indextype

  # Calculate summary statistics
  stats <- list(
    RecordingGivid = recording,
    indextype = indextype,
    n_total_types = nrow(object),
    best_match = object$syntaxonCode[1],
    best_value = index_values[1],
    top_matches = top_matches_format,
    value_stats = stats::summary(index_values)
  )

  class(stats) <- "summary.inbovegclassification"
  return(stats)
}

#' Summary method for inbovegclassification_list Objects
#'
#' @description
#' Applies `summary()` to each classification in the list.
#'
#' @param object list of classifications
#' @param ... arguments passed to summary.inbovegclassification
#'
#' @return A list of summary objects
#' @export
summary.inbovegclassification_list <- function(object, ...) {
  lapply(object, summary, ...)
}
