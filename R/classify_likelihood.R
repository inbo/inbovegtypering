#' Calculate Likelihood Index for Vegetation Classification
#'
#' @description
#' Calculates the maximum likelihood index to classify vegetation plots
#' against reference syntaxa,
#' following the ASSOCIA method (van Tongeren et al. 2008).
#' This function processes one record at a time.
#'
#' @param data A dataframe containing the plot data with columns:
#'   \itemize{
#'     \item RecordingGivid: Unique identifier for the recording
#'     \item LayerCode: Code identifying vegetation layer
#'     \item CoverageCode: Code for coverage type
#'     \item PctValue: Percentage cover value
#'     \item gbif_usageKey: GBIF species identifier
#'   }
#' on wich recordings are identified.
#' When NULL all data is assumed from same recording
#' @param synoptics A dataframe containing reference syntaxa data with columns:
#'   \itemize{
#'     \item syntaxoncode: Code identifying the vegetation type
#'     \item usageKey: GBIF species identifier
#'     \item frequentie: Frequency of species in vegetation type (0-1)
#'   }
#'  @param normalised ignored logical variable
#'
#' @return A dataframe with columns:
#'   \itemize{
#'     \item syntaxoncode: Vegetation type code
#'     \item likelihood: -2ln(likelihood) value,
#'     where lower values indicate better matches
#'   }
#'
#' @details
#' The function:
#' 1. Filters for herb layer ('K')
#' 2. Converts percentage cover to fractions
#' 3. Calculates likelihood components for present and absent species
#' 4. Sums likelihood values by syntaxon
#'
#' @references
#' van Tongeren, O., Gremmen, N., & Hennekens, S. (2008). Assignment of relevés
#' to pre-defined classes by supervised clustering of plant communities
#' using a new composite index.
#' Journal of Vegetation Science, 19(4), 525-536.
#'
#' @importFrom dplyr left_join select filter mutate group_by summarise arrange
#' @importFrom dplyr join_by select
#' @importFrom tidyr nest unnest
#' @examples
#' # Example data structure
#' data <- data.frame(
#'   RecordingGivid = 1,
#'   LayerCode = "K",
#'   CoverageCode = "PCT",
#'   PctValue = 50,
#'   species_number = 123
#' )
#'
#' synoptics <- data.frame(
#'   syntaxonCode = "A1",
#'   speciesNumber = 123,
#'   frequency = 0.8,
#'   mean_if_present = 2.4
#' )
#'
#' classify_likelihood(data, synoptics)
#'
#' classify_likelihood(data, synoptics = "default")
#'
#'\dontrun{
#' con_taxa <- connect_db_taxonomy()
#' test_record <- example_recordings |>
#'   select(1:12) |>
#'   link_taxon_info(con_taxa = con_taxa)
#'
#' classification <- classify_likelihood(test_record, synoptics = "default")
#'}
#' @export
classify_likelihood <- function(data,
                                synoptics = "default",
                                normalised = TRUE) {
  synoptic_table <- assert_correct_synoptics(synoptics)
  if (synoptics == "default") {
    synoptic_table <-
      utils::getFromNamespace("synoptic_table", "inbovegtypering")
  }

  combined <- synoptic_table |>
    left_join(
      data |>
        unnest(cols = species_number) |> #species_number is a list col (several synoptic matches possible)
        select(
          "RecordingGivid", "LayerCode",
          "CoverageCode", "PctValue",
          "species_number"
        ) |>
        filter(.data$LayerCode == "K") |>
        mutate(fraction = .data$PctValue / 100),
      by = join_by(x$speciesNumber == y$species_number)
    ) |>
    mutate(
      presence = !is.na(.data$fraction) & .data$fraction > 0,
      log_component = case_when(
        presence ~ -2 * log(pmax(.data$fraction, 0.0001)),
        !presence ~ -2 * log(pmax(1 - .data$fraction, 0.0001))
      )
    )

  # Calculate likelihood for each record_id and syntaxon combination
  rv <- combined |>
    group_by(across(all_of(c("RecordingGivid", "syntaxonCode")))) |>
    summarise(
      likelihood = sum(.data$log_component, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(across(all_of("RecordingGivid"))) |>
    arrange(desc(.data$likelihood), .by_group = TRUE)

  # Split into list by RecordingGivid and apply class/attributes to each element
  classification_list <- rv |>
    group_split() |>
    map(function(x) {
      attr(x, "indextype") <- "likelihood"
      class(x) <- c("inbovegclassification", class(x))
      x
    })

  # Name the list elements by RecordingGivid
  names(classification_list) <- unique(rv$RecordingGivid)
  class(classification_list) <-
    c("inbovegclassification_list", class(classification_list))

  classification_list
}
