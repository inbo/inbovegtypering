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
#' @param data A dataframe containing species records
#' @param column Character string specifying the column name in 'data'
#' that contains the species names to match
#'
#' @return A dataframe containing the original data
#' with an additional column
#'   'gbif_usageKey' that contains the GBIF taxonomy keys for matched species
#'
#' @details
#' The function performs the following steps:
#' 1. Extracts unique species names from the specified column
#' 2. Queries the TaxonSourceTaxonGbifMatch table for matches with FUTON source
#' 3. Performs partial string matching to link species names
#' 4. Returns the original data enriched with GBIF taxonomy keys
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
#' result <- link_record_to_taxa_db(con, species_data, "species_name")
#' }
#'
#' @seealso
#' \code{\link{connect_db_taxonomy}} for establishing the database connection
#'
#' @importFrom dplyr left_join filter select distinct mutate tbl sql collect
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#'
#' @export
link_record_to_taxa_db <- function(con, data, column) {
  species_names <- data.frame(name = unique(data[[column]])) |>
    mutate(search_pattern = paste0(.data$name, "%"))

  species_patterns <- paste(
    paste0(
      "TaxonNameExact LIKE '",
      species_names$name, "%'"
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
    collect()

  result <- species_names |>
    left_join(
      taxons |>
        mutate(name_match =
                 substr(.data$TaxonNameExact,
                        1,
                        nchar(.data$TaxonNameExact))) |>
        filter(.data$name_match %in% species_names$name),
      by = join_by(.data$name == .data$name_match)
    ) |>
    select("name", "gbif_usageKey") |>
    distinct()

  join_cols <- setNames("name", column)

  rv <- data |> left_join(result, by = join_cols)
  if (nrow(rv) != nrow(data)) {
    warning("possibly multiple gbif matches for a name")
  }
  rv
}
