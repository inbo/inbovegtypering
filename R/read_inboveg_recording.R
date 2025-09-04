#' Read a recording from inboveg
#'
#' Read a recording from Cydonia, specifying the survey and RecordingGivid
#' @param con_inboveg db connection to Cydonia
#' @param con_taxa db connection to "Taxa" db
#' @param survey survey name
#' @param code releve code within survey
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con_inboveg <- connect_db_inboveg()
#' con_taxa <- connect_db_taxonomy()
#' record <-
#'   read_inboveg_recording(con_inboveg, con_taxa,
#'     survey = "MILKLIM_Heischraal2012",
#'     code = "IV2012081611384756"
#'   )
#' }
read_inboveg_recording <- function(con_inboveg, con_taxa, survey, code) {

  #step 1: reqd inboveg data
  #--------------------------------

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
                                    collect = TRUE)
  } else if (!missing(survey)) {
    records <-
      inbodb::get_inboveg_recording(con_inboveg,
                                  survey_name = survey,
                                  collect = TRUE)
  } else if (!missing(code)) {
    records <-
      inbodb::get_inboveg_recording(con_inboveg,
                                  recording_givid = code,
                                  collect = TRUE)
  } else {
    stop("survey or code must be specified")
  }

  species_names <- records |> select("OriginalName", "ScientificName")

  #step 2: link Recordings to taxonomy
  #------------------------------------

  rv <- link_taxon_info(con_taxa, records)
  rv
}
