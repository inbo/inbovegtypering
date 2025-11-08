#' Post-process Classification Results
#'
#' Formats a results tibble into a named list of 'inbovegclassification' objects,
#' compatible with your S3 plot and summary methods.
#'
#' @param results_df The tibble of classification results.
#' @param index_name The name of the index column (e.g., "Weirdness").
#' @return A list of class 'inbovegclassification_list'.
#' @keywords internal
post_process_classification <- function(results_df, index_name) {
  # Arrange by best match and get the name of the main column
  index_sym <- sym(index_name)
  rv <- results_df |>
    arrange(!!index_sym, .by_group = TRUE)

  # Split into a list, one element per RecordingGivid
  classification_list <- rv |>
    group_split() |>
    purrr::map(function(x) {
      attr(x, "indextype") <- index_name
      attr(x, "RecordingGivid") <- x$RecordingGivid[[1]]
      class(x) <- c("inbovegclassification", class(x))
      x
    })

  names(classification_list) <- unique(rv$RecordingGivid)
  class(classification_list) <- c("inbovegclassification_list", class(classification_list))

  classification_list
}
