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
  records <-
    inbodb::get_inboveg_recording(con_inboveg, survey, collect = TRUE) |>
    dplyr::filter(
      .data$RecordingGivid == code,
      .data$LayerCode == "K"
    ) |>
    dplyr::mutate(coverage = .data$PctValue / 100)
  # link record to usagekey
  records <- link_record_to_taxa_db(con_taxa,
    records,
    column = "OriginalName"
  )
  records
}
