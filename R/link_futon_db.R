#' Link species records to GBIF taxonomy via INBO taxonomy database
#'
#' @description
#' This function links species records to their corresponding
#'GBIF taxonomy information
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
#' @seealso
#' \code{\link{connect_db_taxonomy}} for establishing the database connection
#'
#' @importFrom dplyr left_join filter select distinct mutate tbl sql collect
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#' @importFrom rgbif name_usage
#' @export
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con <- connect_db_taxonomy()
#' species_names <- c("Dryopteris", "Nardus stricta L.",
#'                    "Lophocolea", "Fraxinus excelsior L.")
#' link_futon_db(con, species_names = species_names)
#' }
#'
link_futon_db <- function(con, species_names) {
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
    mutate(name_match = substr(.data$TaxonNameExact,
                               1,
                               nchar(.data$TaxonNameExact))) |>
    filter(.data$name_match %in% species_names)

  result <- data.frame(name = species_names) |>
    left_join(taxons, join_by(name == name_match)
    )

  result <- result |>
    select("name",
           usageKey = "gbif_usageKey") |>
    distinct()

  usage_keys <- na.omit(unique(result$usageKey))
  tree <- sapply(usage_keys, function(x) rgbif::name_usage(x)$data) |>
    bind_rows() |>
    select(usageKey = "nubKey",
           acceptedUsageKey = any_of("acceptedKey"),
           "scientificName",
           "speciesKey", "genusKey", "familyKey",
           "orderKey", "classKey", "phylumKey", "kingdomKey") |>
    mutate(acceptedUsageKey = if("acceptedKey" %in% names(.data)) {
      tree$acceptedKey
    } else {
      NA_integer_
    })

  rv <- result |>
    left_join(tree, join_by(usageKey == usageKey))
  rv
}
