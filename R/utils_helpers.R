#' Prepare Data for Classification
#'
#' This helper function filters recordings, prepares the synoptic table, and
#' creates the full analysis data frame by combining plots against all syntaxa.
#'
#' @param data The raw recording data frame. Must have `RecordingGivid`,
#'   `species_number`, and `PctValue`.
#' @param synoptic_table The validated synoptic table data frame.
#' @param layer The vegetation layer to filter for (default "K").
#'   Note: `data` should already be filtered if this is not desired.
#'   The `read_inboveg_recording` function provides a `LayerCode` column.
#' @return A prepared data frame for classification calculations.
#' @keywords internal
#' @importFrom dplyr filter select distinct mutate right_join
#' @importFrom dplyr rename left_join
#' @importFrom tidyr crossing
create_analysis_data <- function(data, synoptic_table, layer = "K") {
  # 1. Prepare recordings
  # Ensure species_number is a simple vector (not a list)
  # This handles the list-column issue from link_taxon_info
  recordings_prepared <- data |>
    dplyr::filter(.data$LayerCode == layer) |>
    # The select statement was causing the column to be dropped. Removing it.
    dplyr::mutate(
      # Handle list column by taking the first match.
      # A more complex strategy might be needed if multiple matches are common.
      # RENAME to speciesNumber (camelCase) to match synoptic table
      speciesNumber = purrr::map_int(.data$species_number, ~ .x[[1]])
    ) |>
    dplyr::filter(!is.na(.data$speciesNumber)) |>
    # Aggregate duplicate species entries (e.g., from different layers/names)
    # Use the maximum PctValue
    dplyr::group_by(.data$RecordingGivid, .data$speciesNumber) |>
    dplyr::summarise(
      PctValue = max(.data$PctValue, na.rm = TRUE),
      .groups = "drop"
    )

  # 2. Prepare synoptics
  # Frequencies are 0-100 in source, convert to 0-1
  synoptics_prepared <- synoptic_table |>
    dplyr::mutate(frequency = .data$frequency / 100) |>
    dplyr::rename(characteristic_abundance = .data$mean_if_present) |>
    dplyr::select(
      .data$syntaxonCode, .data$speciesNumber,
      .data$frequency, .data$characteristic_abundance
    )

  # 3. Create the full analysis frame
  # Get all unique relevés and all unique species from the prepared data
  unique_plots <- unique(recordings_prepared$RecordingGivid)
  unique_species <- unique(synoptics_prepared$speciesNumber)

  # Create a "full grid" of all species for all plots
  # This is crucial for correctly identifying absent species
  full_grid <- tidyr::crossing(
    RecordingGivid = unique_plots,
    speciesNumber = unique_species
  ) |>
    # Add the PctValue (will be NA for absent species)
    dplyr::left_join(
      recordings_prepared,
      by = c("RecordingGivid", "speciesNumber")
    )

  # 4. Join grid with synoptic table
  # This creates the final data: one row for every species in every syntaxon
  # for every relevé.
  analysis_df <- synoptics_prepared |>
    dplyr::right_join(
      full_grid,
      by = "speciesNumber",
      relationship = "many-to-many" # Each species is in many syntaxa
    )

  return(analysis_df)
}

#' Post-process Classification Results
#'
#' Formats a results tibble into a named list of 'inbovegclassification' objects,
#' compatible with S3 plot and summary methods.
#'
#' @param results_df The tibble of classification results.
#' @param index_name The name of the index (e.g., "Weirdness").
#' @return A list of class `inbovegclassification_list`.
#' @keywords internal
#' @importFrom dplyr arrange group_split
#' @importFrom purrr map
post_process_classification <- function(results_df, index_name) {
  # Get the symbolic name of the index column
  index_sym <- rlang::sym(index_name)

  # Arrange by best match (lowest value)
  rv <- results_df |>
    dplyr::group_by(.data$RecordingGivid) |>
    dplyr::arrange(!!index_sym, .by_group = TRUE)

  # Get the group keys (RecordingGivids) before splitting
  group_keys <- dplyr::group_keys(rv) |> dplyr::pull(.data$RecordingGivid)

  # Split into a list, one element per RecordingGivid
  classification_list <- rv |>
    dplyr::group_split(.keep = FALSE) |> # .keep=FALSE to drop RecordingGivid
    purrr::map2(group_keys, function(x, givid) {
      # Re-add attributes
      attr(x, "indextype") <- index_name
      attr(x, "RecordingGivid") <- givid # Add the Givid attribute
      class(x) <- c("inbovegclassification", "data.frame")
      x
    })

  # Name the list elements by RecordingGivid
  names(classification_list) <- group_keys
  class(classification_list) <-
    c("inbovegclassification_list", "list")

  classification_list
}

#' Check correct synoptics format
#'
#' @param x A data.frame to be assessed as a synoptic table.
#' @return Throws an error if format is incorrect, otherwise returns `TRUE`.
#' @keywords internal
assert_correct_synoptics <- function(x) {
  # Define required columns and their expected types (using class)
  required_cols <- c(
    "syntaxonCode" = "character",
    "speciesNumber" = "numeric", # or integer
    "frequency" = "numeric",
    "mean_if_present" = "numeric"
  )

  # Validate data frame
  if (!is.data.frame(x)) {
    stop("Synoptics must be a data frame", call. = FALSE)
  }

  # Check columns
  missing_cols <- setdiff(names(required_cols), names(x))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Synoptics is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  # Check data types
  for (col in names(required_cols)) {
    expected_type <- required_cols[col]
    actual_class <- class(x[[col]])

    # Allow integer for numeric, but not vice-versa if strict
    if (expected_type == "numeric" &&
      !any(c("numeric", "integer") %in% actual_class)) {
      stop(sprintf(
        "Column '%s' has incorrect data type. Expected '%s', got '%s'",
        col, expected_type, actual_class[1]
      ), call. = FALSE)
    }
    if (expected_type == "character" && actual_class[1] != "character") {
      stop(sprintf(
        "Column '%s' has incorrect data type. Expected '%s', got '%s'",
        col, expected_type, actual_class[1]
      ), call. = FALSE)
    }
  }

  # Check value ranges
  if (any(x$frequency < 0 | x$frequency > 100, na.rm = TRUE)) {
    stop("Frequency values must be between 0 and 100", call. = FALSE)
  }
  if (any(x$mean_if_present < 0 | x$mean_if_present > 100, na.rm = TRUE)) {
    stop("mean_if_present values must be between 0 and 100", call. = FALSE)
  }

  return(TRUE)
}

#' Assert Database Recording Format
#'
#' This function checks if the provided data matches the expected format
#' for database recordings.
#'
#' @param data A tibble or data frame containing the database recording data.
#' @return Returns TRUE if the data matches the expected format.
#'         Otherwise, it throws an error.
#' @keywords internal
#' @importFrom tibble is_tibble
assert_db_recording_format <- function(data) {
  # Define expected column names
  expected_names <- c(
    "RecordingGivid",
    "LayerCode",
    "CoverCode",
    "ScientificName",
    "PctValue"
    # ... add other critical columns from your 'all_scripts.R' example if needed
  )

  # Check column names
  missing_cols <- setdiff(expected_names, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Data is missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Check if PctValue is numeric and within a reasonable range
  if (!is.numeric(data$PctValue)) {
    stop("PctValue column must be numeric")
  }
  if (any(data$PctValue < 0, na.rm = TRUE) ||
    any(data$PctValue > 100, na.rm = TRUE)) {
    warning("PctValue contains values outside the 0-100 range")
  }

  # Check for multiple RecordingGivids
  if (length(unique(data$RecordingGivid)) == 0) {
    stop("No RecordingGivid found in data.")
  }

  return(TRUE)
}
