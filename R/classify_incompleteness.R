#' Classify based on Incompleteness Index
#'
#' Implements Equation 6 from van Tongeren et al. (2008) [cite_start][cite: 1].
#' Calculates the sum of contributions from species absent from the relevé.
#'
#' @param data A dataframe of plot data.
#' @param synoptics A dataframe of synoptic data.
#' @param ... Additional arguments (not used).
#' @return An object of class 'inbovegclassification_list'.
classify_incompleteness <- function(data, synoptics, ...) {
  base_df <- create_analysis_data(data, synoptics)

  results <- base_df |>
    mutate(
      presence = !is.na(PctValue),
      freq_safe = pmax(frequency, 0.0001)
    ) |>
    filter(!presence) |> # Only species absent from relevé contribute
    group_by(RecordingGivid, syntaxonCode) |>
    summarise(
      Incompleteness = sum(-2 * log(1 - freq_safe), na.rm = TRUE),
      .groups = "drop"
    )

  post_process_classification(results, "Incompleteness")
}
