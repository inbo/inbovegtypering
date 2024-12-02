#' Link species records to GBIF taxonomy via INBO flora database
#'
#' @description
#' This function links species records to their corresponding
#'florabank taxonomy information
#' by matching species names against the INBO taxonomy database (FUTON source).
#' It performs a partial string match to accommodate variations
#' in species name notation.
#'
#' @param con A database connection object to the INBO taxonomy database
#' @param species_names A character vector containing species records

#' @return A dataframe containing the matched species with the futondb
#'
#' @seealso
#' \code{\link{connect_db_flora}} for establishing the database connection
#'
#' @importFrom dplyr left_join filter select distinct mutate tbl sql collect
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#' @importFrom rgbif name_usage
#' @export
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con <- connect_db_flora()
#' species_names <- c("Dryopteris", "Nardus stricta L.",
#'                    "Lophocolea", "Fraxinus excelsior L.")
#' link_futon_db(con, species_names = species_names)
#' }
#'
link_flora_db <- function(con, species_names) {
  rv
}
