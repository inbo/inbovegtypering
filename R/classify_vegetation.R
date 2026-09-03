#' Classify Vegetation Recordings
#'
#' Calculates similarity indices
#' (Weirdness, Incompleteness, Likelihood, MED, CoD)
#' for vegetation recordings against a synoptic table of syntaxa.
#'
#' @param data A data frame containing the vegetation recordings.
#' Must contain columns:
#'   `recording` (ID), `species_number`, `pct_value` (cover percentage 0-100),
#'   and `layer_code`.
#' @param synoptics A data frame, a file path, or the string "default".
#'   If a data frame, it must contain columns:
#'   `syntaxon_code`, `species_number`,
#'   `frequency` (0-100), and `mean_if_present` (0-100).
#'   If "default" or "package", the function looks for a file named
#'   `synoptic_table.csv` in the package's `extdata` directory.
#' @param layer Character string indicating the layer code to filter
#'  from `data`.
#'   Defaults to "ALL", ( other examples "K" (Herb layer)).
#' @param r Numeric. Weighting parameter for the "Weirdness" component in the
#'   Composite Distance (CoD). Default is 0.50.
#' @param s Numeric. Weighting exponent for the qualitative part (Likelihood) in
#'   the CoD. Default is 0.60.
#' @param t Numeric. Weighting parameter for the presence/absence balance in the
#'   Modified Euclidean Distance (MED). Default is 0.50.
#'
#' @return An object of class `inbovegclassification_list`, which is a list of
#'   objects of class `inbovegclassification` (one per recording).
#'
#' @importFrom dplyr filter mutate group_by summarise transmute
#' @importFrom dplyr left_join do ungroup
#' @importFrom tidyr crossing replace_na
#' @importFrom purrr map_int
#' @importFrom assertthat assert_that has_name is.readable
#' @importFrom utils read.csv
#' @export
classify_vegetation <- function(data,
                                synoptics,
                                layer = "ALL",
                                r = 0.50,
                                s = 0.60,
                                t = 0.50) {
  # --- Assertions (Inputs) ---
  assertthat::assert_that(
    is.data.frame(data),
    assertthat::has_name(data, c("recording",
                                 "species_number",
                                 "pct_value",
                                 "layer_code")),
    is.numeric(r), is.numeric(s), is.numeric(t)
  )

  # --- Preparation ---

  # 1. Process Synoptics (Load file/package data or validate dataframe)
  # We do this first to ensure we have a valid dataframe before proceeding
  synoptics_clean <- prepare_synoptic_table(synoptics)

  # 2. Process recordings (filter layer, handle list-cols, aggregate)
  recordings_clean <- prepare_recordings(data, layer = layer)

  # 3. Join to create the full analysis grid (Cartesian product per recording)
  # This ensures we have rows for species absent in the recording
  # but present in syntaxon
  joined_data <- join_recordings_synoptics(recordings_clean, synoptics_clean)

  # --- Calculation ---
  # Calculates indices per recording-syntaxon combination
  results_df <- calculate_indices(joined_data, r = r, s = s, t = t)

  # --- Output Formatting ---
  # Split results by recording ID to create a list structure
  results_list <- split(results_df, results_df$recording)

  # Convert each element to an 'inbovegclassification' object
  classification_objects <- lapply(results_list, function(x) {
    structure(
      list(
        recording_id = unique(x$recording),
        results = x,
        parameters = list(layer = layer, r = r, s = s, t = t)
      ),
      class = "inbovegclassification"
    )
  })

  # Return the list with a specific class
  structure(classification_objects, class = "inbovegclassification_list")
}
