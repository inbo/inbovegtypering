#' Classify based on Weirdness Index
#'
#' Implements Equation 5 from van Tongeren et al. (2008) [cite_start][cite: 1].
#' Calculates the sum of contributions from species present in the relevé.
#'
#' @param data A dataframe of plot data.
#' @param synoptics A dataframe of synoptic data.
#' @param ... Additional arguments (not used).
#' @return An object of class 'inbovegclassification_list'.
classify_weirdness <- function(data, synoptics, ...) {
  base_df <- create_analysis_data(data, synoptics)

  results <- base_df |>
    mutate(
      presence = !is.na(PctValue),
      freq_safe = pmax(frequency, 0.0001)
    ) |>
    filter(presence) |> # Only species present in relevé contribute
    group_by(RecordingGivid, syntaxonCode) |>
    summarise(
      Weirdness = sum(-2 * log(freq_safe), na.rm = TRUE),
      .groups = "drop"
    )
  post_process_classification(results, "Weirdness")
}
