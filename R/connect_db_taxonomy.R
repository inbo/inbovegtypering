#' Connect to INBO taxonomy database
#'
#' @description
#' Creates a database connection to the INBO taxonomy database.
#' This function is a wrapper around \code{connect_inbo_dbase()}
#' specifically for connecting to the taxonomy database.
#'
#' @param db Character string specifying the database name.
#' Defaults to "D0155_00_Taxa".
#' @param test flag whether the test modus is used
#'
#' @return A database connection object
#'
#' @details
#' This function establishes a connection to the INBO taxonomy database
#' which contains standardized
#' taxonomic information. The database contains authoritative taxonomic data
#' that can be used
#' for standardizing species names and checking taxonomic classifications.
#' @importFrom inbodb connect_inbo_dbase
#' @section Database:
#' The taxonomy database (D0155_00_Taxa) contains standardized
#' taxonomic information used by INBO.
#' @export
#' @param db The name of the INBO hosted database. Defaults to Taxa db
#'
#' @return connection object
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_db_taxonomy <- function(db = "D0155_00_Taxa", test = FALSE) {
  if (!test) {
    tryCatch(
      {
        con <- connect_inbo_dbase(db)
        return(con)
      },
      error = function(e) {
        message("Could not connect to taxa database,
                using test database instead")
      }
    )
  }
  test_db_path <- system.file("testdata", "test_taxa.sqlite",
    package = "inbovegtypering"
  )
  if (!file.exists(test_db_path)) {
    # Create test database if it doesn't exist
    create_test_inboveg_database()
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  con
}
