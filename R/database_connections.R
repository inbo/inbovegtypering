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


###############################################################################
#' Create a test database
#'
#' @return inboveg database
create_test_inboveg_database <- function() {
  return("to be implemented")
}


###############################################################################

#' Link species records to GBIF taxonomy via INBO taxonomy database
#'
#' @description
#' This function links species records to their corresponding
#' GBIF taxonomy information
#' by matching species names against the INBO taxonomy database (FUTON source).
#' It performs a partial string match to accommodate variations
#' in species name notation.
#'
#' @param con A database connection object to the INBO taxonomy database
#' @param species_names A character vector containing species records

#' @return A dataframe containing the matched species with the futondb
#'
#' @note
#' - Species names are matched using a 'LIKE' query with wildcard at the end
#' - Only matches from the FUTON source are considered
#' - If no match is found, the gbif_usageKey will be NA
#'
#' @examples
#' \dontrun{
#' # Assuming 'con' is your database connection
#' # and 'species_data' is your dataframe with a column 'species_name'
#' result <- link_to_futondb(con, species_data, "species_name")
#' }
#'
#' @importFrom dplyr left_join filter select distinct mutate tbl sql collect
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames na.omit
#' @importFrom rgbif name_usage
#' @importFrom tibble as_tibble
#' @importFrom dplyr join_by bind_rows any_of
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con <- connect_db_taxonomy()
#' species_names <- c(
#'   "Dryopteris", "Nardus stricta L.",
#'   "Lophocolea", "Fraxinus excelsior L."
#' )
#' link_futon_db(con, species_names = species_names)
#' }
#'
link_taxa_db <- function(con, species_names) {
  species_patterns <- paste(
    paste0(
      "TaxonNameExact LIKE '",
      species_names, "%'"
    ),
    collapse = " OR "
  )

  taxons <- tbl(con, "TaxonSourceTaxonGbifMatch") |>
    filter(sql("taxonsourcename LIKE 'FUTON%'")) |>
    filter(sql(paste0("(", species_patterns, ")"))) |>
    select(
      "TaxonNameExact",
      "gbif_usageKey",
      "gbif_matchType",
      "gbif_acceptedusageKey"
    ) |>
    collect() |>
    as_tibble() |>
    mutate(name_match = substr(
      .data$TaxonNameExact,
      1,
      nchar(.data$TaxonNameExact)
    )) |>
    filter(.data$name_match %in% species_names)

  species_data <- data.frame(name = species_names) |>
    left_join(taxons, join_by(.data$name == .data$name_match))

  result <- species_data |>
    select("name",
      usageKey = "gbif_usageKey"
    ) |>
    distinct()

  usage_keys <- na.omit(unique(result$usageKey))
  if (!length(usage_keys)) {
    stop("No non-NA usageKeys found")
  }
  tree <- sapply(usage_keys, function(x) rgbif::name_usage(x)$data) |>
    bind_rows() |>
    select(
      usageKey = "nubKey",
      acceptedUsageKey = any_of("acceptedKey"),
      "scientificName",
      "speciesKey", "genusKey", "familyKey",
      "orderKey", "classKey", "phylumKey", "kingdomKey"
    ) |>
    mutate(acceptedUsageKey = if ("acceptedKey" %in% names(.data)) {
      tree$acceptedKey
    } else {
      NA_integer_
    })

  rv <- result |>
    left_join(tree, join_by(.data$usageKey == .data$usageKey))
  rv
}
