#' Classify based on Modified Euclidean Distance (MED)
#'
#' Implements Equation 8 from van Tongeren et al. (2008) [cite_start][cite: 1].
#' Calculates a quantitative distance based on species abundance.
#'
#' @param data A dataframe of plot data.
#' @param synoptics A dataframe of synoptic data.
#' @param t Weight for absent species in MED (default 1).
#' @param ... Additional arguments (not used).
#' @return An object of class 'inbovegclassification_list'.
classify_euclidean <- function(data, synoptics, t = 1, ...) {
  base_df <- create_analysis_data(data, synoptics)

  results <- base_df |>
    mutate(
      presence = !is.na(PctValue),
      avg_abundance_in_type = characteristic_abundance * frequency,
      med_comp_present = if_else(presence, (PctValue - characteristic_abundance)^2, 0),
      med_comp_absent = if_else(!presence, (t * avg_abundance_in_type)^2, 0)
    ) |>
    group_by(RecordingGivid, syntaxonCode) |>
    summarise(
      MED = sqrt(sum(med_comp_present, na.rm = TRUE) + sum(med_comp_absent, na.rm = TRUE)),
      .groups = "drop"
    )

  post_process_classification(results, "MED")
}
