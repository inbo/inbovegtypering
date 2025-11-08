#' Load species list
#'
#' @param source source where to find the species list
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @export
#' @return tibble with important species fields
#'
load_species_list <- function(source = "package") {
  species_list <- NULL
  if (source == "package") {
    species_list <- read_csv2(
      file.path(
        system.file(package = "inbovegtypering"),
        "species_list",
        "gbif_inbo.csv"))
  } else if (source == "testdata") {
    #voorbereide testdataset, gebaseerd op bv een 10-tal synoptische types
  } else if (source == "development") {
    species_list <- read_csv2("inst/species_list/gbif_inbo.csv") |>
      filter(.data$status == "ACCEPTED") |>
      select(
        .data$TaxonGIVID, .data$usageKey, .data$TaxonName,
        .data$TaxonLanguageKey, .data$TaxonQuickCode,
        .data$scientificName, .data$canonicalName,
        .data$kingdom, .data$family, .data$genus, .data$genusKey,
        .data$species, .data$speciesKey
      )
  } else {
    stop("no valid source selected to load species list from")
  }
  species_list
}
