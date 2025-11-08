#' Link Species Information with Taxonomic Data
#'
#' @description
#' Links species records with taxonomic information from various sources:
#' 1. Local taxonomic database
#' 2. GBIF backbone taxonomy
#' 3. Custom species list
#' The function attempts to match species names first using original names,
#' then scientific names, and finally using external services for unmatched species.
#'
#' @param data A data frame containing species records with columns 'OriginalName'
#'             and 'ScientificName'
#' @param con_taxa A database connection object for the taxonomic database
#'
#' @return A data frame containing the original records enriched with taxonomic
#'         information, including:
#'         - usageKey: Taxonomic identifier
#'         - acceptedUsageKey: Identifier of accepted name (if different)
#'         - species_number: List column with matching synoptic species numbers
#' @export
#' @details
#' The matching process follows these steps:
#' 1. Matches original names against local taxonomy database
#' 2. For unmatched names, tries matching scientific names
#' 3. Remaining unmatched names are looked up in GBIF backbone
#' 4. Finally checks against a custom species list
#' The function creates a list column 'species_number' containing all possible
#' matches from both usage keys and names.
#'
#' @importFrom dplyr filter select pull slice bind_rows left_join rowwise mutate
#' @importFrom rgbif name_backbone_checklist
#'
#' @examples
#' \dontrun{
#' library(inbovegtypering)
#' con <- connect_db_taxonomy()
#' test <- example_recordings |> select(1:12)
#' linked_records <- link_taxon_info(con, test)
#' }
#'
link_taxon_info <- function(con_taxa, data) {
  # Extract species names from records
  species_names <- data |>
    select("OriginalName", "ScientificName")

  # First attempt: match original names
  species_def_o <- link_taxa_db(con_taxa,
                                species_names |> pull(OriginalName))
  which_unclassified <- which(is.na(species_def_o$usageKey))

  # Second attempt: match scientific names for unmatched records
  species_def_s <- link_taxa_db(con_taxa,
                                species_names |> pull(ScientificName))

  # Combine successful matches from both attempts
  species_def <- species_def_o |>
    filter(!is.na(usageKey)) |>
    bind_rows(species_def_s |> slice(which_unclassified))

  # Ensure acceptedUsageKey column exists
  if(!("acceptedUsageKey" %in% colnames(species_def)))
    species_def$acceptedUsageKey <- NA

  # Get remaining unmatched names
  unclassified_names <- species_def |>
    filter(is.na(usageKey)) |>
    pull(name)

  if(length(unclassified_names)) {
    # Try GBIF backbone for unmatched names
    rgbif_found <- rgbif::name_backbone_checklist(unclassified_names) |>
      rename(name = "verbatim_name")

    if(!("acceptedUsageKey" %in% colnames(rgbif_found)))
      rgbif_found$acceptedUsageKey <- NA

    rgbif_found <- rgbif_found |>
      select(all_of(colnames(species_def)))

    species_def <- species_def |>
      filter(!is.na(usageKey)) |>
      bind_rows(rgbif_found)
  }

  # Check for any remaining unmatched names
  unclassified_names2 <- species_def |>
    filter(is.na(usageKey)) |>
    pull(name)

  # Final attempt: check custom species list
  if (length(unclassified_names2)) {
    in_specieslist <- link_species_list(unclassified_names2)
    species_def <- species_def |>
      filter(!is.na(usageKey)) |>
      bind_rows(in_specieslist)
    }

  # Generate species numbers by both usage key and name
  on_usagekey <- species_def |>
    rowwise() |>
    mutate(
      species_number_uk = list(link_synoptic_species(usageKey,
                                                     which = "usage_key")),
      species_number_nm = list(link_synoptic_species(name,
                                                     which = "name")),
      species_number = list(
        unique(c(na.omit(as.numeric(unlist(species_number_uk))),
                 na.omit(as.numeric(unlist(species_number_nm)))))))

  # Join results back to original records
  rv <- data |>
    left_join(on_usagekey, join_by(OriginalName == name))

  return(rv)
}
