#' Create a dataset ready for classification
#'
#' @param data data with recordings
#' @param synoptics synoptic table when data.frame or label to get the table
#'
#' @returns combined data.frame with all necessary info for classification
#' @export
#'
create_classification_data <- function(data, synoptics) {
  if (inherits(synoptics, "data.frame")) {
    synoptic_table <- synoptics
  } else if (synoptics == "default") {
    synoptic_table <-
      utils::getFromNamespace("synoptic_table", "inbovegtypering")
  }
  synoptic_table <- assert_correct_synoptics(synoptics)
  synoptic_table |>
    left_join(
      data |>
        unnest(cols = species_number) |> # is list(multiple matches possible)
        select(
          "RecordingGivid", "LayerCode",
          "CoverageCode", "PctValue",
          "species_number"
        ) |>
        filter(.data$LayerCode == "K") |>
        mutate(fraction = .data$PctValue / 100),
      by = join_by(x$speciesNumber == y$species_number)
    )
}
