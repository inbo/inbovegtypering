#' Calculate Incompleteness Index
#'
#' @description
#' Calculates incompleteness index following van Tongeren et al. (2008)
#'
#' @inheritParams classify_likelihood
#' @return An inbovegclassification object
#' @export
classify_incompleteness <- function(data, synoptics) {
  combined <- synoptics |>
    left_join(
      data |>
        select(
          "RecordingGivid", "LayerCode",
          "CoverageCode", "PctValue",
          "gbif_usageKey"
        ) |>
        filter(.data$LayerCode == "K") |>
        mutate(fraction = .data$PctValue / 100),
      by = join_by(.data$usageKey == .data$gbif_usageKey)
    ) |>
    mutate(
      presence = !is.na(.data$fraction) & .data$fraction > 0,
      log_component = case_when(
        !.data$presence ~ -2 * log(1 - pmin(.data$frequentie, 0.9999)),
        TRUE ~ 0
      )
    ) # Only use absent species

  rv <- combined |>
    group_by(.data$syntaxoncode) |>
    summarise(incompleteness = sum(.data$log_component)) |>
    arrange(.data$incompleteness)

  attr(rv, "indextype") <- "incompleteness"
  class(rv) <- c("inbovegclassification", class(rv))
  rv
}
