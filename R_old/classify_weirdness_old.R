#' Calculate Weirdness Index
#'
#' @description
#' Calculates weirdness index following van Tongeren et al. (2008)
#'
#' @inheritParams classify_likelihood
#' @return An inbovegclassification object
#' @export
classify_weirdness <- function(data,
                               synoptics = "default",
                               normalised = TRUE) {
  if (inherits(synoptics, "data.frame")) {
    synoptic_table <- synoptics
  } else if (synoptics == "default") {
    synoptic_table <-
      utils::getFromNamespace("synoptic_table", "inbovegtypering")
  }
  synoptic_table <- assert_correct_synoptics(synoptics)

  combined <- synoptic_table |>
    left_join(
      data |>
        unnest(cols = species_number) |> # species_number is a list col (several synoptic matches possible)
        select(
          "RecordingGivid", "LayerCode",
          "CoverageCode", "PctValue",
          "species_number"
        ) |>
        filter(.data$LayerCode == "K") |>
        mutate(fraction = .data$PctValue / 100),
      by = join_by(x$speciesNumber == y$species_number)
    )
  print(combined)
  combined <- combined |>
    mutate(
      presence = !is.na(.data$fraction) & .data$fraction > 0,
      log_component = case_when(
        presence ~ -2 * log(pmax(.data$frequency, 0.0001)),
        TRUE ~ 0
      )
    ) # Only use present species

  rv <- combined |>
    group_by(.data$syntaxonCode) |>
    summarise(weirdness = sum(.data$log_component)) |>
    arrange(.data$weirdness)

  attr(rv, "indextype") <- "weirdness"
  class(rv) <- c("inbovegclassification_list", class(rv))
  rv
}
