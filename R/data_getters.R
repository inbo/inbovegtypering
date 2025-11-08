#' Read a recording from inboveg
#'
#' Read a recording from Cydonia, specifying the survey and RecordingGivid,
#' and link it to taxonomic information.
#'
#' @param con_inboveg db connection to Cydonia (inboveg).
#' @param con_taxa db connection to "Taxa" db.
#' @param survey survey name.
#' @param code releve code (RecordingGivid) within survey.
#'
#' @return A data.frame with the relevé data, linked to taxonomic info
#'   (including `species_number`).
#' @export
#' @importFrom inbodb get_inboveg_recording
#' @importFrom dplyr select
read_inboveg_recording <- function(con_inboveg, con_taxa, survey, code) {
  # Check that at least one of survey or code is present
  if (missing(survey) && missing(code)) {
    stop("At least one of 'survey' or 'code' must be specified")
  }

  # Both survey and code specified
  if (!missing(survey) && !missing(code)) {
    records <-
      inbodb::get_inboveg_recording(con_inboveg,
        survey_name = survey,
        recording_givid = code,
        collect = TRUE
      )
  } else if (!missing(survey)) {
    records <-
      inbodb::get_inboveg_recording(con_inboveg,
        survey_name = survey,
        collect = TRUE
      )
  } else if (!missing(code)) {
    records <-
      inbodb::get_inboveg_recording(con_inboveg,
        recording_givid = code,
        collect = TRUE
      )
  }

  if (nrow(records) == 0) {
    warning("No records found for the given survey/code.")
    return(invisible(NULL))
  }

  # Step 2: link Recordings to taxonomy
  rv <- link_taxon_info(con_taxa, records)
  rv
}

#' Read a species list from the taxonomy database
#'
#' Retrieves the species list (TaxonSourceTaxonGbifMatch) from the
#' taxonomy database.
#'
#' @param con_taxa db connection to "Taxa" db.
#'
#' @return A tibble (from dbplyr) with the species list.
#' @export
#' @importFrom dplyr tbl
read_inboveg_specieslist <- function(con_taxa) {
  # This query is from beschrijving.Rmd
  taxa <- dplyr::tbl(con_taxa, "TaxonSourceTaxonGbifMatch")
  # You may want to add filtering or collecting here, e.g.:
  # taxa <- dplyr::tbl(con_taxa, "TaxonSourceTaxonGbifMatch") |>
  #   dplyr::filter(taxonsourcename == "FUTON") |>
  #   dplyr::collect()
  return(taxa)
}

#' Get the synoptic table
#'
#' Loads the synoptic table from the package's internal data or a user-provided
#' path.
#'
#' @param synoptics A data frame with the synoptic table, the string
#'   "default" to use the package's default table, or a file path
#'   to a .csv2 file.
#'
#' @return A data frame containing the synoptic table, validated to have
#'   the correct columns and data types.
#' @export
#' @importFrom readr read_csv2
#' @importFrom utils getFromNamespace
get_synoptic_table <- function(synoptics) {
  if (is.data.frame(synoptics)) {
    synoptic_table <- synoptics
  } else if (identical(synoptics, "default")) {
    # This assumes your default table is stored as package data
    # (e.g., in data/synoptic_table.rda or as an internal object)
    # Using getFromNamespace as in your script:
    synoptic_table <-
      utils::getFromNamespace("synoptic_table", "inbovegtypering")
  } else if (is.character(synoptics) && file.exists(synoptics)) {
    synoptic_table <- readr::read_csv2(synoptics)
  } else {
    stop(
      "Invalid 'synoptics' argument. Must be a data.frame, 'default', ",
      "or a valid file path."
    )
  }

  # Validate the table structure
  assert_correct_synoptics(synoptic_table)

  return(synoptic_table)
}
