#' Calculate Modified Euclidean Distance
#'
#' @description
#' Calculates modified Euclidean distance following van Tongeren et al. (2008)
#' @importFrom dplyr case_when filter mutate join_by
#' @inheritParams classify_likelihood
#' @param t Weight for absent species (0-1, default 0.5)
#' @return An inbovegclassification object
#' @export
classify_euclidean <- function(data, synoptics, t = 0.5) {
  stopifnot(t >= 0 && t <= 1)

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
      # Calculate characteristic abundance
      CA = .data$gem_als_aanwezig, # Assuming this column exists in synoptics
      # Calculate squared differences
      sq_diff = case_when(
        presence ~ (fraction - CA)^2,
        !presence ~ t * (CA)^2
      )
    )

  rv <- combined |>
    group_by(.data$syntaxoncode) |>
    summarise(euclidean = sqrt(sum(.data$sq_diff))) |>
    arrange(.data$euclidean)

  attr(rv, "indextype") <- "euclidean"
  class(rv) <- c("inbovegclassification", class(rv))
  rv
}
