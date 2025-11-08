#' Classify based on the Composite Distance (CoD)
#'
#' Implements Equation 9 from van Tongeren et al. (2008) [cite_start][cite: 1].
#' Combines Weirdness, Incompleteness, and MED into a single index.
#'
#' @param data A dataframe of plot data.
#' @param synoptics A dataframe of synoptic data.
#' @param r Weight for Weirdness component (default 1.5).
#' @param s Power for MED component (default 0.60).
#' @param ... Additional arguments (not used).
#' @export
#' @return An object of class 'inbovegclassification_list'.
classify_cod <- function(data, synoptics, r = 1.5, s = 0.60, ...) {
  base_df <- create_analysis_data(data, synoptics)

  results <- base_df |>
    mutate(
      presence = !is.na(PctValue),
      freq_safe = pmax(frequency, 0.0001),
      weirdness_comp = if_else(presence, -2 * log(freq_safe), 0),
      incompleteness_comp = if_else(!presence, -2 * log(1 - freq_safe), 0),
      avg_abundance_in_type = characteristic_abundance * frequency,
      med_comp_present = if_else(presence, (PctValue - characteristic_abundance)^2, 0),
      med_comp_absent = if_else(!presence, avg_abundance_in_type^2, 0)
    ) |>
    group_by(RecordingGivid, syntaxonCode) |>
    summarise(
      Weirdness = sum(weirdness_comp, na.rm = TRUE),
      Incompleteness = sum(incompleteness_comp, na.rm = TRUE),
      MED = sqrt(sum(med_comp_present, na.rm = TRUE) + sum(med_comp_absent, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      qualitative_part = (r * Weirdness + Incompleteness) / r,
      CoD = qualitative_part * (MED^s),
      indextype = "CoD"
    )

  results <<- results
  post_process_classification(results, "CoD")
}
