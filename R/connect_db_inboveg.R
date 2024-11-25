#' Connect to the INBO database
#'
#' @param db The name of the INBO hosted database. Defaults to Cydonia db
#' @param test flag whether the test modus is used
#'
#' @return connection object
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_db_inboveg <- function(db = "D0010_00_Cydonia", test = FALSE) {
  if (!test) {
    tryCatch(
      {
        con <- connect_inbo_dbase(db)
        return(con)
      },
      error = function(e) {
        message("Could not connect to inboveg database,
                using test database instead")
      }
    )
  }
  test_db_path <- system.file("testdata", "test_cydonia.sqlite",
    package = "inbovegtypering"
  )
  if (!file.exists(test_db_path)) {

    # Create test database if it doesn't exist
    create_test_inboveg_database()
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  con
}
