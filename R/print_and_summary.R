#' Print method for inbovegclassification
#' @param x inbovegclassification object
#' @param ... arguments passed to cat
#' @export
print.inbovegclassification <- function(x, ...) {
  cat("INBO vegetation classification object\n")
  cat("Recording ID:", x$recording_id, "\n")
  cat("Best matching syntaxa:\n")
  print(head(x$results[
    order(x$results$cod),
    c("syntaxon", "wrd", "inc", "llk", "med", "cod")
  ]))
}

#' Print method for inbovegclassification_list
#' @param x inbovegclassification_list object
#' @param ... arguments passed to cat
#' @export
print.inbovegclassification_list <- function(x, ...) {
  cat("List of INBO vegetation classifications\n")
  cat("Number of recordings classified:", length(x), "\n")
}

#' Summary for a Single Vegetation Classification
#'
#' Summarizes the classification results for a single recording. Allows sorting
#' by specific indices and selecting specific columns to display.
#'
#' @param object An object of class `inbovegclassification`.
#' @param sort_by Character string. The column name to sort by (ascending).
#'   Defaults to "cod".
#' @param indices Character vector.
#' The indices (columns) to display in the output.
#'   Defaults to c("cod", "nrm_cod", "med", "nrm_llk", "nrm_wrd", "nrm_inc").
#'   Available options are the columns present in `object$results`, including
#'   normalized indices such as "nrm_cod", "nrm_llk", "nrm_wrd", and "nrm_inc".
#' @param top_n Integer. The number of best matching syntaxa to return.
#'   Defaults to 20. Set to Inf to return all.
#' @param digits Number of digits to display per index, defaults to 3
#' @param ... Additional arguments (not used).
#'
#' @return A data frame (invisibly) containing the sorted and filtered results,
#'   with additional class `summary.inbovegclassification` for pretty printing.
#'
#' @importFrom dplyr arrange select all_of
#' @importFrom utils head
#' @importFrom assertthat assert_that has_name is.count
#' @export
summary.inbovegclassification <- function(object,
                                          sort_by = "cod",
                                          indices = c("cod", "nrm_cod", "med",
                                                      "nrm_llk", "nrm_wrd",
                                                      "nrm_inc"),
                                          top_n = 20,
                                          digits = 3,
                                          ...) {
  # --- Assertions ---
  assertthat::assert_that(
    assertthat::has_name(object$results, sort_by),
    msg = paste("Column", sort_by, "not found in results.")
  )

  missing_indices <- setdiff(indices, colnames(object$results))
  if (length(missing_indices) > 0) {
    stop(paste("Indices not found in results:",
               paste(missing_indices, collapse = ", ")))
  }

  assertthat::assert_that(
    assertthat::is.number(top_n) || is.infinite(top_n)
  )

  # --- Sorting and Selection ---
  # Ensure syntaxon is always included for context
  cols_to_select <- unique(c("syntaxon", indices))

  summary_df <- object$results |>
    dplyr::arrange(.data[[sort_by]]) |>
    dplyr::select(dplyr::all_of(cols_to_select)) |>
    utils::head(top_n)

  # Attach metadata for the print method
  attr(summary_df, "recording_id") <- object$recording_id
  attr(summary_df, "sorted_by") <- sort_by

  class(summary_df) <- c("summary.inbovegclassification", class(summary_df))

  return(summary_df)
}

#' Print method for summary.inbovegclassification
#' @param x inbovegclassification object
#' @param ... not used
#' @export
print.summary.inbovegclassification <- function(x, ...) {
  rec_id <- attr(x, "recording_id")
  sort_col <- attr(x, "sorted_by")

  cat("Classification Summary for Recording:", rec_id, "\n")
  cat("Sorted by:", sort_col, "(ascending/best fit first)\n")
  cat("Top", nrow(x), "matches:\n\n")

  # Print the dataframe part using standard print
  print.data.frame(x)
  invisible(x)
}

#' Summary for a List of Vegetation Classifications
#'
#' Summarizes results for multiple recordings. Allows filtering for specific
#' recording IDs and passing sorting parameters to the individual summaries.
#'
#' @param object An object of class `inbovegclassification_list`.
#' @param recording_ids Character vector. Specific recording IDs to summarize.
#'   If NULL (default), all recordings in the list are summarized.
#' @param ... Arguments passed to `summary.inbovegclassification`
#'   (e.g., `sort_by`, `indices`, `top_n`, `digits`).
#'
#' @return A list of summaries (class `summary.inbovegclassification_list`).
#'
#' @importFrom assertthat assert_that
#' @export
summary.inbovegclassification_list <- function(object,
                                               recording_ids = NULL,
                                               ...) {
  # --- Filter List ---
  if (!is.null(recording_ids)) {
    # Check if requested IDs exist
    available_ids <- names(object)
    missing_ids <- setdiff(recording_ids, available_ids)

    if (length(missing_ids) > 0) {
      warning(paste(
        "The following recording IDs were not found:",
        paste(missing_ids, collapse = ", ")
      ))
    }

    # Subset the list
    # Use intersect to safe subset only existing keys
    keys_to_use <- intersect(recording_ids, available_ids)

    if (length(keys_to_use) == 0) {
      stop("No valid recording IDs found in the object.")
    }

    subset_obj <- object[keys_to_use]
  } else {
    subset_obj <- object
  }

  # --- Apply Summary to Elements ---
  res_list <- lapply(subset_obj, function(x) {
    summary(x, ...)
  })

  class(res_list) <- "summary.inbovegclassification_list"
  return(res_list)
}

#' Print method for summary.inbovegclassification_list
#' @param x inbovegclassification_list object
#' @param ... not used
#' @export
print.summary.inbovegclassification_list <- function(x, ...) { #nolint
  # If the list is long, we might not want to print everything automatically
  # But standard R behavior is usually to print the list contents.

  if (length(x) == 0) {
    cat("Empty classification summary list.\n")
    return(invisible(x))
  }

  for (i in seq_along(x)) {
    print(x[[i]])
    cat("\n--------------------------------------------------------\n\n")
  }
  invisible(x)
}
