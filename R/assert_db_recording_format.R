#' Assert Database Recording Format
#'
#' This function checks if the provided data matches the expected format
#' for database recordings. It verifies the structure, column names, and
#' data types of the input tibble.
#'
#' @param data A tibble containing the database recording data.
#'
#' @return Returns TRUE if the data matches the expected format.
#'         Otherwise, it throws an error with a descriptive message.
#'
#' @details
#' The function checks for the following:
#' \itemize{
#'   \item The input is a tibble
#'   \item The tibble has exactly 13 columns
#'   \item All expected columns are present with correct data types
#'   \item The PctValue column contains numeric values between 0 and 100
#'   \item The RecordingScale is consistent across all rows
#' }
#' @importFrom tibble is_tibble
#' @export
#'
assert_db_recording_format <- function(data) {
  # Check if it's a tibble
  if (!inherits(data, "tbl_df")) {
    stop("Data is not a tibble")
  }

  # Check number of columns
  if (ncol(data) != 13) {
    stop("Data does not have 13 columns")
  }

  # Define expected column names and types
  expected_columns <- c(
    Name = "character",
    RecordingGivid = "character",
    UserReference = "character",
    LayerCode = "character",
    CoverCode = "character",
    OriginalName = "character",
    ScientificName = "character",
    TaxonGroupCode = "character",
    PhenologyCode = "character",
    Comment = "character",
    CoverageCode = "character",
    PctValue = "numeric",
    RecordingScale = "character"
  )

  # Check column names and types
  for (col_name in names(expected_columns)) {
    if (!col_name %in% names(data)) {
      stop(paste("Column", col_name, "is missing"))
    }
    if (typeof(data[[col_name]]) != expected_columns[col_name]) {
      stop(paste(
        "Column", col_name, "is not of type",
        expected_columns[col_name]
      ))
    }
  }

  # Check if PctValue is numeric and within a reasonable range
  if (!is.numeric(data$PctValue) ||
    any(data$PctValue < 0) ||
    any(data$PctValue > 100)) {
    stop("PctValue should be numeric and between 0 and 100")
  }

  # Check if RecordingScale is consistent
  if (length(unique(data$RecordingScale)) != 1) {
    stop("RecordingScale is not consistent across all rows")
  }

  # If all checks pass, return TRUE
  return(TRUE)
}
