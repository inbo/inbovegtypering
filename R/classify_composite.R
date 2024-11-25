#' Calculate Composite Distance
#'
#' @description
#' Calculates composite distance combining likelihood, weirdness, incompleteness
#' and modified Euclidean distance following van Tongeren et al. (2008)
#'
#' @inheritParams classify_likelihood
#' @param r Weight for present species (1-2, default 1.5)
#' @param s Power weight for composition vs cover (0-1, default 0.6)
#' @param t Weight for absent species in Euclidean distance (0-1, default 0.5)
#' @importFrom dplyr left_join mutate select arrange
#' @importFrom rlang .data
#' @return An inbovegclassification object
#' @export
classify_composite <- function(data, synoptics, r = 1.5, s = 0.6, t = 0.5) {
  stopifnot(r >= 1 && r <= 2)
  stopifnot(s >= 0 && s <= 1)
  stopifnot(t >= 0 && t <= 1)

  # Get individual components
  w <- classify_weirdness(data, synoptics)
  i <- classify_incompleteness(data, synoptics)
  e <- classify_euclidean(data, synoptics, t)

  # Combine into composite index
  rv <- w |>
    left_join(i, by = "syntaxoncode") |>
    left_join(e, by = "syntaxoncode") |>
    mutate(
      composite = ((r * .data$weirdness + .data$incompleteness) / r)^s *
        .data$euclidean^(1 - s)
    ) |>
    select("syntaxoncode", "composite") |>
    arrange(.data$composite)

  attr(rv, "indextype") <- "composite"
  class(rv) <- c("inbovegclassification", class(rv))
  rv
}
