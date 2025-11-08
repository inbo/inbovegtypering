#' Calculate Weirdness Index (Eq. 5)
#'
#' Internal function to calculate the Weirdness index.
#' Calculates the sum of contributions from species *present* in the relevé.
#'
#' @param analysis_df A data frame prepared by `create_analysis_data()`.
#' @param ... Additional arguments (not used).
#' @return An object of class `inbovegclassification_list`.
#' @keywords internal
classify_weirdness <- function(analysis_df, ...) {
  results <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      freq_safe = pmax(.data$frequency, 0.0001)
    ) |>
    dplyr::filter(.data$presence) |> # Only species present in relevé
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      Weirdness = sum(-2 * log(.data$freq_safe), na.rm = TRUE),
      .groups = "drop"
    )

  post_process_classification(results, "Weirdness")
}

#' Calculate Incompleteness Index (Eq. 6)
#'
#' Internal function to calculate the Incompleteness index.
#' Calculates the sum of contributions from species *absent* from the relevé.
#'
#' @param analysis_df A data frame prepared by `create_analysis_data()`.
#' @param ... Additional arguments (not used).
#' @return An object of class `inbovegclassification_list`.
#' @keywords internal
classify_incompleteness <- function(analysis_df, ...) {
  results <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      # Handle frequency of 1 (100%) for absent species
      freq_safe = pmin(.data$frequency, 0.9999)
    ) |>
    dplyr::filter(!.data$presence) |> # Only species absent from relevé
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      Incompleteness = sum(-2 * log(1 - .data$freq_safe), na.rm = TRUE),
      .groups = "drop"
    )

  post_process_classification(results, "Incompleteness")
}

#' Calculate -2ln(Likelihood) Index (Eq. 4)
#'
#' Internal function to calculate the likelihood index.
#' This is the sum of the Weirdness and Incompleteness indices.
#'
#' @param analysis_df A data frame prepared by `create_analysis_data()`.
#' @param ... Additional arguments (not used).
#' @return An object of class `inbovegclassification_list`.
#' @keywords internal
classify_likelihood <- function(analysis_df, ...) {
  # -2LL (Eq. 4) is the sum of Weirdness (Eq. 5) and Incompleteness (Eq. 6)

  # 1. Calculate Weirdness component
  weirdness_df <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      freq_safe = pmax(.data$frequency, 0.0001)
    ) |>
    dplyr::filter(.data$presence) |>
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      W = sum(-2 * log(.data$freq_safe), na.rm = TRUE),
      .groups = "drop"
    )

  # 2. Calculate Incompleteness component
  incompleteness_df <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      freq_safe = pmin(.data$frequency, 0.9999)
    ) |>
    dplyr::filter(!.data$presence) |>
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      I = sum(-2 * log(1 - .data$freq_safe), na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Combine them
  # Full join to handle cases where all species are present (no I)
  # or all are absent (no W)
  results <- dplyr::full_join(weirdness_df, incompleteness_df,
    by = c("RecordingGivid", "syntaxonCode")
  ) |>
    dplyr::mutate(
      W = ifelse(is.na(.data$W), 0, .data$W),
      I = ifelse(is.na(.data$I), 0, .data$I),
      Likelihood = .data$W + .data$I
    ) |>
    dplyr::select(.data$RecordingGivid, .data$syntaxonCode, .data$Likelihood)

  post_process_classification(results, "Likelihood")
}


#' Calculate Modified Euclidean Distance (MED) (Eq. 8)
#'
#' Internal function to calculate the Modified Euclidean Distance.
#'
#' @param analysis_df A data frame prepared by `create_analysis_data()`.
#' @param t Weight for absent species in MED (default 1, as per paper).
#' @param ... Additional arguments (not used).
#' @return An object of class `inbovegclassification_list`.
#' @keywords internal
classify_med <- function(analysis_df, t = 1, ...) {
  results <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      # PctValue is 0-100, characteristic_abundance is 0-100
      avg_abundance_in_type = .data$characteristic_abundance * .data$frequency,
      med_comp_present = ifelse(presence,
        (.data$PctValue - .data$characteristic_abundance)^2,
        0
      ),
      med_comp_absent = ifelse(!presence,
        (t * avg_abundance_in_type)^2,
        0
      )
    ) |>
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      MED = sqrt(sum(.data$med_comp_present, na.rm = TRUE) +
        sum(.data$med_comp_absent, na.rm = TRUE)),
      .groups = "drop"
    )

  post_process_classification(results, "MED")
}

#' Calculate Composite Distance (CoD) (Eq. 9)
#'
#' Internal function to calculate the Composite Distance.
#'
#' @param analysis_df A data frame prepared by `create_analysis_data()`.
#' @param r Weight for Weirdness component (default 1.5).
#' @param s Power for MED component (default 0.60).
#' @param ... Additional arguments (not used).
#' @return An object of class `inbovegclassification_list`.
#' @keywords internal
classify_cod <- function(analysis_df, r = 1.5, s = 0.60, ...) {
  results <- analysis_df |>
    dplyr::mutate(
      presence = !is.na(.data$PctValue) & .data$PctValue > 0,
      # Qualitative components
      freq_safe_present = pmax(.data$frequency, 0.0001),
      freq_safe_absent = pmin(.data$frequency, 0.9999),
      weirdness_comp = ifelse(presence,
        -2 * log(.data$freq_safe_present),
        0
      ),
      incompleteness_comp = ifelse(!presence,
        -2 * log(1 - .data$freq_safe_absent),
        0
      ),
      # Quantitative components
      avg_abundance_in_type = .data$characteristic_abundance * .data$frequency,
      med_comp_present = ifelse(presence,
        (.data$PctValue - .data$characteristic_abundance)^2,
        0
      ),
      med_comp_absent = ifelse(!presence,
        .data$avg_abundance_in_type^2,
        0
      )
    ) |>
    dplyr::group_by(.data$RecordingGivid, .data$syntaxonCode) |>
    dplyr::summarise(
      Weirdness = sum(.data$weirdness_comp, na.rm = TRUE),
      Incompleteness = sum(.data$incompleteness_comp, na.rm = TRUE),
      MED = sqrt(sum(.data$med_comp_present, na.rm = TRUE) +
        sum(.data$med_comp_absent, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      qualitative_part = (r * .data$Weirdness + .data$Incompleteness) / r,
      # Handle MED = 0 cases
      CoD = .data$qualitative_part * (pmax(.data$MED, 0.0001)^s)
    ) |>
    dplyr::select(
      .data$RecordingGivid, .data$syntaxonCode, .data$CoD,
      .data$Weirdness, .data$Incompleteness, .data$MED
    )

  post_process_classification(results, "CoD")
}
