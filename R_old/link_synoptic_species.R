
#' Try to find the corresponding species_number in de synoptic table
#'
#' @param x record for which the synoptic species number must be found.
#' Max 1 row. Containing at least columns usageKey and Originalname
#' @param which character controlling what is to be tested
#' possibilities are "usage_key", "genus_key", "name"
#' @importFrom dplyr filter pull
#' @return data.frame with column speciesNumber
#' @export
#'
#' @examples
#' library(inbovegtypering)
#' test <- data.frame(OriginalName = "Lemna trisulca", usageKey = 2867579)
#' link_synoptic_species(test$usageKey, which = "usage_key")
#' link_synoptic_species(test$OriginalName, which = "name")
#'
#' test2 <- data.frame(name = c("Potamogeton pectinatus","Algenvlokken",
#'                              "Carex", "Carex Nonexistans"))
#' test2 |>
#' rowwise() |>
#' mutate(speciesNumber = link_synoptic_species(name, which = "name"))
#'
link_synoptic_species <- function(x, which = "name") {

  species_list <- utils::getFromNamespace("species_list", "inbovegtypering")
  if (length(x)  == 0) return(NA_integer_)
  if (length(x) != 1) {
    warning("multiple rows, only first used")
    x <- x[1]
  }
  if (which == "usage_key") {
    species_number <- species_list |>
      filter(.data$usageKey == x |  .data$acceptedUsageKey == x) |>
      pull(.data$speciesNumber)
    if(!length(species_number)) return(NA_integer_) else return(species_number)
  }

  if (which == "genus_key") {
    species_number <- species_list |>
      filter(.data$genusKey == x) |>
      pull(.data$speciesNumber)
    if(!length(species_number)) return(NA_integer_) else return(species_number)
  }

  if (which == "name") {
   species_number <- species_list |>
    filter(.data$speciesName == x | .data$scientificName == x) |>
    pull(.data$speciesNumber)
  if(!length(species_number)) return(NA_integer_) else return(species_number)
  }

  rv
}
