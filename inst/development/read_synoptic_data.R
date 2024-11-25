utils::globalVariables(".data")

#' Read Synoptic Info
#'
#' This function reads the synoptic data from a CSV file
#' in the package's inst/soortenlijst folder.
#'
#' @param code character vector with syntaxon code names
#' to retrieve the information from.
#' When NULL all synoptic data for each syntaxon is returned
#' @return A data frame containing the syntaxon information.
#' @importfrom rlang .data
#' @examples
#' # example code
#' species_list <- read_species_list()
#' synoptics <- read_synoptic_data()
#' synoptics <- read_synoptic_data(code = "12AA01C")
#'
#' @export
read_synoptic_data <- function(code = NULL, specieslist) {
  # Get the path to the CSV file
  file_path <- system.file("soortenlijst",
    "synoptische_gegevens_2005.csv",
    package = "inbovegtypering"
  )

  # Check if the file exists
  if (file_path == "") {
    stop("Synoptic data file not found in the package.")
  }

  # Read the CSV file
  synoptic_data <- readr::read_csv2(file_path)

  # check code argument
  if (!is.null(code)) {
    synoptic_data <- synoptic_data |>
      dplyr::filter(.data$syntaxoncode %in% code)
    if (!nrow(synoptic_data)) {
      stop("argument code did not cover existing syntaxon codes")
    }
  }

  # check if column names match
  cols <- c(
    "syntaxoncode",
    "soortnummer",
    "frequentie",
    "gem_als_aanwezig"
  )

  if (!all(cols %in% colnames(synoptic_data))) {
    stop("Synoptic data has not the right format")
  }

  # return data
  return(synoptic_data)
}
