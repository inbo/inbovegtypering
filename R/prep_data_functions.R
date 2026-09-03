#' Prepare Synoptic Table
#'
#' Handles loading of the synoptic table
#' from various sources (package, file, dataframe),
#' checks required column names,
#' and ensures data types are correct for calculation.
#'
#' @param synoptics A data frame, file path (string), or "default"/"package".
#'
#' @return A data frame with standardized column names for internal calculation.
#' @noRd
prepare_synoptic_table <- function(synoptics) {
  # 1. Handle Input Types (String vs Data Frame)
  if (is.character(synoptics)) {
    if (length(synoptics) != 1) {
      stop("synoptics argument must be a single string or a data frame")
    }

    if (synoptics %in% c("default", "package")) {
      # Load from package inst/extdata
      file_path <- system.file("extdata",
                               "synoptic_table.csv",
                               package = "inbovegtypering")
      if (file_path == "") {
        stop("Default synoptic_table.csv not found in package extdata.")
      }
      synoptics <- utils::read.csv(file_path, stringsAsFactors = FALSE)
    } else {
      # Treat as file path
      assertthat::assert_that(assertthat::is.readable(synoptics))
      synoptics <- utils::read.csv(synoptics, stringsAsFactors = FALSE)
    }
  }

  # 2. Validate Data Frame Structure
  if (!is.data.frame(synoptics)) {
    stop("synoptics must be a data frame or a valid file path.")
  }

  # Check required columns (using the NEW names provided by user)
  required_cols <- c("syntaxon_code",
                     "species_number",
                     "frequency",
                     "mean_if_present")
  missing_cols <- setdiff(required_cols, colnames(synoptics))

  if (length(missing_cols) > 0) {
    stop(paste("Synoptic table is missing columns:",
               paste(missing_cols, collapse = ", ")))
  }

  # 3. Type Conversion and Renaming for Internal Logic
  synoptics_out <- synoptics |>
    dplyr::mutate(
      species_number = as.integer(.data$species_number)
    ) |>
    # We explicitly select and rename here to match the variable names
    # expected by calculate_indices() and join_recordings_synoptics().
    # Note: syntaxon_code and species_number are kept as is.
    # frequency and mean_if_present are mapped
    # to pct_presence and cover_if_present
    # to maintain compatibility with the calculation functions.
    dplyr::transmute(
      syntaxon_code = .data$syntaxon_code,
      species_number = .data$species_number,
      pct_presence = .data$frequency,
      cover_if_present = .data$mean_if_present
    )

  return(synoptics_out)
}

#' Prepare Recordings Data
#'
#' Clean and aggregate recording data for analysis.
#'
#' @param data Raw data frame of recordings.
#' @param layer Layer code to filter.
#'
#' @return A summarized data frame with one row per recording-species combi.
#' @noRd
prepare_recordings <- function(data, layer) {
  if (layer != "ALL") {
    data <- data |>
      dplyr::filter(.data$layer_code == layer)
  }
  data <- data |>
    dplyr::mutate(
      # Handle possible list column by taking the first match (if applicable)
      speciesNumber = if (is.list(.data$species_number)) {
        purrr::map_int(.data$species_number, ~ .x[[1]])
      } else {
        .data$species_number
      }
    ) |>
    dplyr::filter(!is.na(.data$species_number)) |>
    dplyr::group_by(.data$recording, .data$species_number) |>
    # If duplicate entries exist for a species in a layer,
    #take the maximum cover
    dplyr::summarise(
      pct_value = max(.data$pct_value, na.rm = TRUE),
      .groups = "drop"
    )
  data
}

#' Join Recordings and Synoptics
#'
#' Creates a full grid of recordings and syntaxa species to ensure proper
#' calculation of absence/presence indices.
#'
#' @param recordings Prepared recordings data frame.
#' @param synoptics Prepared synoptics data frame.
#'
#' @return A joined data frame where missing species in recordings are filled
#' with 0 cover.
#' @noRd
join_recordings_synoptics <- function(recordings, synoptics) {
  # Create a grid of all recordings x all syntaxon entries
  # We need the syntaxon structure to define the 'universe'
  # of species for that syntaxon
  tidyr::crossing(
    data.frame(recording = unique(recordings$recording)),
    synoptics
  ) |>
    dplyr::left_join(recordings, by = c("recording", "species_number")) |>
    dplyr::mutate(pct_value = tidyr::replace_na(.data$pct_value, 0))
}
