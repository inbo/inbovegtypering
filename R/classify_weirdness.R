#' Calculate Weirdness Index
#'
#' @description
#' Calculates weirdness index following van Tongeren et al. (2008)
#'
#' @inheritParams classify_likelihood
#' @return An inbovegclassification object
#' @export
classify_weirdness <- function(data, synoptics) {
  combined <- synoptics |>
    left_join(
      data |>
        select(
          "RecordingGivid", "LayerCode",
          "CoverageCode", "PctValue",
          "usageKey"
        ) |>
        filter(.data$LayerCode == "K") |>
        mutate(fraction = .data$PctValue / 100),
      by = join_by(.data$gbif_usageKey == .data$usageKey)
    ) |>
    mutate(
      presence = !is.na(.data$fraction) & .data$fraction > 0,
      log_component = case_when(
        presence ~ -2 * log(pmax(.data$frequentie, 0.0001)),
        TRUE ~ 0
      )
    ) # Only use present species

  rv <- combined |>
    group_by(.data$syntaxoncode) |>
    summarise(weirdness = sum(.data$log_component)) |>
    arrange(.data$weirdness)

  attr(rv, "indextype") <- "weirdness"
  class(rv) <- c("inbovegclassification", class(rv))
  rv
}
