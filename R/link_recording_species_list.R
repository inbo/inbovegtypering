#' Link recording to species list
#'
#' @description
#' This function links species records to their corresponding
#'GBIF taxonomy information needed in this package
#'
#' @param con A database connection object to the INBO taxonomy database
#' @param data A dataframe containing recordings
#' @param col_name Character string specifying the column name in 'data'
#' that contains the species names to match
#' @param source where to find the taxonomic information:
#' "included" the usageKey and acceptedUsageKey are already present.
#' Used to check if the data is compatible for classification
#' "taxon_db" INBO taxon db,
#' "gbif_backbone" get from via gbif backbone api,
#' "data.frame" data.frame containing the variables
#' @param taxons the data.frame containing name, usageKey, acceptedUsageKey
#' semicolon separated, with these names: name, usageKey, acceptedUsageKey
#' @param max_gbif integer >= 0 indicating the max species provided to gbif,
#' so not accidentally thousands of records are requested by api. Default 5000.
#'
#' @return A dataframe containing the original data
#' with an additional column
#'   'gbif_usageKey' that contains the GBIF taxonomy keys for matched species
#'
##' @examples
#' \dontrun{
#' # Assuming 'con' is your database connection
#' # and 'species_data' is your dataframe with a column 'species_name'
#' library(inbovegtypering)
#' con <- connect_db_taxonomy()
#' link_recording_species_list(
#'   example_recordings |> select(2, 4, 6, 12),
#'   "OriginalName",
#'   con = con,
#'   source = "taxon_db")
#'
#' test_records <- example_recordings |> slice(1:5) |> select(2, 4, 6, 12)
#' link_recording_species_list(test_records, col_name = "OriginalName",
#'                             source = "gbif_backbone")
#' }
#'
#' library(inbovegtypering)
#' test_records <- example_recordings |> slice(c(1:3,5)) |> select(2, 4, 6, 12)
#' species_data <- data.frame(name = c("Acer platanoides L.",
#'                                     "Acer pseudoplatanus L.",
#'                                     "Achillea millefolium L.",
#'                                     "Achillea ptarmica L."),
#'                            usageKey = c(3189846, 3189870, 3120060, 3120333),
#'                            acceptedUsageKey = NA)
#' taxons <-
#'   lapply(species_data$usageKey,
#'          FUN = function(x) {rgbif::name_usage(x)$data}) |>
#'   bind_rows() |>
#'   right_join(species_data, join_by(key == usageKey)) |>
#'   mutate(usageKey = key,
#'          acceptedUsageKey = NA_character_)
#' link_recording_species_list(test_records, col_name = "OriginalName",
#'                             source = "data.frame", taxons = taxons)
#'
#' @seealso
#' \code{\link{connect_db_taxonomy}} for establishing the database connection
#'
#' @importFrom dplyr left_join filter select distinct mutate tbl sql collect
#' @importFrom DBI dbGetQuery
#' @importFrom stats setNames
#'
#' @export
link_recording_species_list <- function(
                                   data,
                                   col_name,
                                   con = NULL,
                                   source = "taxon_db",
                                   taxons = NULL,
                                   max_gbif = 5000) {

  #validate input
  if (! inherits(data, "data.frame")) stop("data not a valid data.frame")
  species_names <- unique(data[[col_name]])
  if (!length(species_names)) stop("col_name does not exist in the dataset")

  taxonomy_keys <-
    c("scientificName", "usageKey", "acceptedUsageKey", "genusKey", "familyKey",
      "orderKey", "classKey", "phylumKey", "kingdomKey")

  #choose which method to link the taxon information
  if (source == "taxon_db") {
    if (is.null(con)) stop("No taxon db connection specified")
    rv <- data |>
      left_join(link_futon_db(con, species_names),
                join_by(!!sym(col_name) == name))
    return(rv)

  } else if (source == "gbif_backbone") {
    if (length(species_names) > max_gbif) {
      stop(paste0(length(species_names),
                  "higher than allowed in max_gbif = ", max_gbif,
                  "\n increase max_gbif to allow this"))
    }
    information <- rgbif::name_backbone_checklist(species_names)
    rv <- data |>
      left_join(information,
                join_by(!!sym(col_name) == verbatim_name))
    if (!("acceptedUsageKey" %in% colnames(rv))) {
      rv <- rv |> mutate(acceptedUsageKey = NA_character_)
    }
    return(rv)

  } else if (source == "data.frame") {
    required_cols <- c("name", taxonomy_keys)
    if (!all(required_cols %in% colnames(taxons))) {
      stop(paste0("GBIF keys ",
                  paste(required_cols, collapse = ", "),
                  " must be present in data"))
    }
    rv <- data |>
      left_join(taxons,
                join_by(!!sym(col_name) == name))
    return(rv)

  } else if (source == "included") {
    if (!all(taxonomy_keys %in% colnames(data))) {
      stop(paste0("GBIF keys ",
                  paste(taxonomy_keys, collapse = ", "),
                  " must be present in data"))
    }
    return(data)
  } else {
    stop("invalid source parameter (see documentation)")
  }
}
