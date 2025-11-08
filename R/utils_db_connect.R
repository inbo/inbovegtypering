#' Connect to the INBOVEG database
#'
#' @param db The name of the INBO hosted database. Defaults to "D0010_00_Cydonia".
#' @param test Flag whether the test modus is used. If TRUE, tries to connect
#'   to a local test database (requires setup).
#'
#' @return A database connection object.
#' @export
#' @importFrom inbodb connect_inbo_dbase
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
connect_db_inboveg <- function(db = "D0010_00_Cydonia", test = FALSE) {
  if (!test) {
    tryCatch(
      {
        con <- inbodb::connect_inbo_dbase(db)
        return(con)
      },
      error = function(e) {
        message("Could not connect to live inboveg database.")
        message("Set test = TRUE to use a test database (if available).")
        stop(e)
      }
    )
  }

  # Logic for test database
  test_db_path <- system.file("testdata", "test_cydonia.sqlite",
    package = "inbovegtypering"
  )
  if (!file.exists(test_db_path)) {
    stop("Test database not found. Run `create_test_inboveg_database()` ",
      "or ensure 'test_cydonia.sqlite' is in 'inst/testdata'.",
      call. = FALSE
    )
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  con
}

#' Connect to INBO taxonomy database
#'
#' @description
#' Creates a database connection to the INBO taxonomy database.
#'
#' @param db Character string specifying the database name.
#' Defaults to "D0155_00_Taxa".
#' @param test Flag whether the test modus is used. If TRUE, tries to connect
#'   to a local test database.
#'
#' @return A database connection object.
#' @export
#' @importFrom inbodb connect_inbo_dbase
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
connect_db_taxonomy <- function(db = "D0155_00_Taxa", test = FALSE) {
  if (!test) {
    tryCatch(
      {
        con <- inbodb::connect_inbo_dbase(db)
        return(con)
      },
      error = function(e) {
        message("Could not connect to live taxonomy database.")
        message("Set test = TRUE to use a test database (if available).")
        stop(e)
      }
    )
  }

  # Logic for test database
  test_db_path <- system.file("testdata", "test_taxa.sqlite",
    package = "inbovegtypering"
  )
  if (!file.exists(test_db_path)) {
    stop("Test database not found. Run `create_test_taxa_database()` ",
      "or ensure 'test_taxa.sqlite' is in 'inst/testdata'.",
      call. = FALSE
    )
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  con
}

#' Connect to INBO flora database
#'
#' @description
#' Creates a database connection to the INBO flora database.
#'
#' @param db Character string specifying the database name.
#' Defaults to "D0012_00_Flora".
#' @param test flag whether the test modus is used
#'
#' @return A database connection object
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_db_flora <- function(db = "D0012_00_Flora", test = FALSE) {
  if (!test) {
    tryCatch(
      {
        con <- inbodb::connect_inbo_dbase(db)
        return(con)
      },
      error = function(e) {
        message("Could not connect to live flora database.")
        message("Set test = TRUE to use a test database (if available).")
        stop(e)
      }
    )
  }

  # Logic for test database
  test_db_path <- system.file("testdata", "test_flora.sqlite",
    package = "inbovegtypering"
  )
  if (!file.exists(test_db_path)) {
    stop("Test database not found. Ensure 'test_flora.sqlite' ",
      "is in 'inst/testdata'.",
      call. = FALSE
    )
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), test_db_path)
  con
}
