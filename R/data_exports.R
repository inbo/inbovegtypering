
#' @export
dummy_recordings <- NULL

#' @export
dummy_species_list <- NULL

#' @export
dummy_synoptics <- NULL

utils::globalVariables(c(
  "syntaxon_code",
  ".",
  "RecordingGivid", # Add any other bare column names you use within dplyr pipes
  "speciesNumber",
  "frequency",
  "mean_if_present",
  "pct_value",
  "layer_code",
  "cover_if_present"
))
