#' Link Species Information with Taxonomic Data
#'
#' @description
#' Links species records with taxonomic information from a local taxonomy
#' database and/or GBIF, then links to the synoptic table's `species_number`.
#'
#' @param con_taxa A database connection object for the taxonomic database.
#' @param data A data frame containing species records with columns 'OriginalName'
#'             and 'ScientificName'.
#'
#' @return A data frame containing the original records enriched with
#'         `species_number` (a list column with matching synoptic species numbers).
#' @keywords internal
#' @importFrom dplyr filter select pull slice bind_rows left_join rowwise mutate
#' @importFrom dplyr distinct
#' @importFrom rgbif name_backbone_checklist
#' @importFrom purrr map
link_taxon_info <- function(con_taxa, data) {
  # Extract unique species names from records
  species_names_df <- data |>
    dplyr::select("OriginalName", "ScientificName") |>
    dplyr::distinct()

  # --- 1. Match OriginalName against taxa DB ---
  species_def_o <- link_taxa_db(
    con_taxa,
    species_names_df |>
      dplyr::pull(.data$OriginalName) |>
      unique()
  ) |>
    dplyr::rename(name = "TaxonNameExact")

  # Join back to get ScientificName
  species_def_o <- species_names_df |>
    dplyr::select(name = "OriginalName", "ScientificName") |>
    dplyr::right_join(species_def_o, by = "name")

  unmatched <- species_def_o |>
    dplyr::filter(is.na(.data$gbif_usageKey))

  # --- 2. Match ScientificName for unmatched records ---
  if (nrow(unmatched) > 0) {
    species_def_s <- link_taxa_db(
      con_taxa,
      unmatched |>
        dplyr::pull(.data$ScientificName) |>
        unique()
    ) |>
      dplyr::rename(name_s = "TaxonNameExact")

    # Join back to original 'unmatched' df
    unmatched <- unmatched |>
      dplyr::select(name = "OriginalName", "ScientificName") |>
      dplyr::left_join(species_def_s,
        by = c("ScientificName" = "name_s")
      )
  }

  # Combine successful matches
  species_def <- species_def_o |>
    dplyr::filter(!is.na(.data$gbif_usageKey)) |>
    dplyr::bind_rows(
      unmatched |> dplyr::filter(!is.na(.data$gbif_usageKey))
    )

  unmatched_final <- unmatched |>
    dplyr::filter(is.na(.data$gbif_usageKey)) |>
    dplyr::select(name = "OriginalName", "ScientificName")

  # --- 3. GBIF backbone for remaining ---
  if (nrow(unmatched_final) > 0) {
    gbif_names <- unique(unmatched_final$ScientificName)
    if (length(gbif_names) > 0) {
      gbif_found <- rgbif::name_backbone_checklist(gbif_names) |>
        dplyr::select(
          name_s = "verbatim_name",
          gbif_usageKey = "usageKey",
          gbif_acceptedusageKey = "acceptedUsageKey"
        )

      gbif_full <- unmatched_final |>
        dplyr::left_join(gbif_found, by = c("ScientificName" = "name_s"))

      species_def <- dplyr::bind_rows(species_def, gbif_full)
    }
  }

  # --- 4. Final Link to species_number ---
  # Ensure 'gbif_acceptedusageKey' exists
  if (!("gbif_acceptedusageKey" %in% colnames(species_def))) {
    species_def$gbif_acceptedusageKey <- NA
  }

  species_def <- species_def |>
    dplyr::rowwise() |>
    dplyr::mutate(
      species_number = list(
        link_synoptic_species(
          .data$gbif_usageKey,
          .data$gbif_acceptedusageKey,
          .data$name
        )
      )
    )

  # Join results back to original data
  rv <- data |>
    dplyr::left_join(
      species_def |>
        dplyr::select(
          name = "OriginalName", "species_number",
          "gbif_usageKey", "gbif_acceptedusageKey"
        ),
      by = c("OriginalName" = "name")
    )

  return(rv)
}


#' Link species names to INBO taxonomy database
#'
#' @param con A database connection object to the INBO taxonomy database.
#' @param species_names A character vector containing species records.
#'
#' @return A dataframe containing the matched species from the futondb.
#' @keywords internal
#' @importFrom dplyr tbl filter select collect as_tibble mutate
#' @importFrom dplyr distinct left_join
#' @importFrom rlang .data
#' @importFrom DBI dbGetQuery
link_taxa_db <- function(con, species_names) {
  if (length(species_names) == 0) {
    return(dplyr::tibble(
      TaxonNameExact = character(),
      gbif_usageKey = integer(),
      gbif_matchType = character(),
      gbif_acceptedusageKey = integer()
    ))
  }

  # Create SQL 'IN' clause
  species_sql <- paste(paste0("'", gsub("'", "''", species_names), "'"),
    collapse = ", "
  )

  # Use FUTON source and exact match
  query_sql <- paste(
    "SELECT TaxonNameExact, gbif_usageKey, gbif_matchType, gbif_acceptedusageKey",
    "FROM TaxonSourceTaxonGbifMatch",
    "WHERE taxonsourcename LIKE 'FUTON%'",
    "AND TaxonNameExact IN (", species_sql, ")"
  )

  taxons <- DBI::dbGetQuery(con, query_sql) |>
    dplyr::as_tibble() |>
    dplyr::distinct() # Ensure unique rows

  return(taxons)
}

#' Find corresponding species_number in the synoptic species list
#'
#' @param usageKey The species' gbif_usageKey.
#' @param acceptedUsageKey The species' gbif_acceptedusageKey.
#' @param name The species' original name (as fallback).
#'
#' @return A vector of matching `speciesNumber` values, or `NA_integer_`
#'   if no match.
#' @keywords internal
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @importFrom utils getFromNamespace
link_synoptic_species <- function(usageKey, acceptedUsageKey, name) {
  # Get the internal species list
  species_list <-
    utils::getFromNamespace("species_list", "inbovegtypering")

  if (!is.na(usageKey)) {
    # Match on usageKey or acceptedUsageKey
    species_number <- species_list |>
      dplyr::filter(.data$usageKey == usageKey |
        .data$acceptedUsageKey == usageKey |
        # Also check if the provided acceptedUsageKey matches
        !is.na(acceptedUsageKey) &
          (.data$usageKey == acceptedUsageKey |
            .data$acceptedUsageKey == acceptedUsageKey)) |>
      dplyr::pull(.data$speciesNumber)

    if (length(species_number) > 0) {
      return(unique(species_number))
    }
  }

  # Fallback to name match
  if (!is.na(name)) {
    species_number <- species_list |>
      dplyr::filter(.data$speciesName == name |
        .data$scientificName == name) |>
      dplyr::pull(.data$speciesNumber)

    if (length(species_number) > 0) {
      return(unique(species_number))
    }
  }

  return(NA_integer_)
}

#' Link species with internal species list
#'
#' @param species_names character vector of species names to be found
#'
#' @return data.frame containing species information
#' @keywords internal
#' @importFrom dplyr left_join
#' @importFrom utils getFromNamespace
link_species_list <- function(species_names) {
  species_list <-
    utils::getFromNamespace("species_list", "inbovegtypering")
  rv <- data.frame(speciesName = species_names) |>
    dplyr::left_join(species_list, by = "speciesName")
  rv
}
