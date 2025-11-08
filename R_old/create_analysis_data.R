#' Prepare Data for Classification
#'
#' This helper function filters recordings, prepares the synoptic table, and
#' creates the full analysis data frame by combining plots against all syntaxa.
#'
#' @param data The raw recording data frame.
#' @param synoptics The raw synoptic table.
#' @param layer The vegetation layer to filter for (e.g., "K").
#' @return A prepared data frame for classification calculations.
#' @export
create_analysis_data <- function(data, synoptics, layer = "K") {
  # 0. Synoptic data
  len_ugivid <- length(unique(data$RecordingGivid))
  if (len_ugivid > 100) {
    message(
      "There are ",
      len_ugivid,
      " Recordings.\n If the routine takes too long or crashes try to use less RecordingGivids at the same time"
    )
  }

  if (inherits(synoptics, "data.frame")) {
    synoptic_table <- synoptics
  } else if (synoptics == "default") {
    synoptic_table <-
      utils::getFromNamespace("synoptic_table", "inbovegtypering")
  } else {
    #expect a path
    synoptic_table <- readr::read_csv2(synoptics)
  }
  synoptic_table <- assert_correct_synoptics(synoptic_table)

  # 1. Prepare recordings (hier nog probleem dat meerdere soorten kunnen in species_number)
  recordings_prepared <- data |>
    filter(LayerCode == layer) |>
    select(RecordingGivid, species_number, PctValue)

  #just temporary fix (should be solved)
  recordings_prepared <- recordings_prepared |>
    mutate(species_number = map_dbl(species_number, ~ .x[[1]]))

  # 2. Prepare synoptics
  synoptics_prepared <- synoptic_table |>
    mutate(frequency = frequency / 100) |>
    rename(characteristic_abundance = mean_if_present)

  # 3. Create the full analysis frame
  unique_plots <- unique(recordings_prepared$RecordingGivid)
  unique_syntaxa <- unique(synoptics_prepared$syntaxonCode)

  tidyr::crossing(
    RecordingGivid = unique_plots,
    syntaxonCode = unique_syntaxa
  ) |>
    left_join(synoptics_prepared, by = "syntaxonCode") |>
    left_join(
      recordings_prepared,
      by = c("RecordingGivid", speciesNumber = "species_number")
    )
}
