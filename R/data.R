#' Example vegetation recordings
#'
#' A dataset containing sample vegetation recordings for testing and examples.
#' The data is retrieved from the INBOVEG database Cydonia.
#'
#' @format A data frame with example variables:
#' \describe{
#'   \item{Name}{Name of the project}
#'   \item{RecordingGivid}{Name of the record}
#'   \item{UserReference}{Naming in the project}
#'   \item{LayerCode}{Layer identifier}
#'   \item{CoverCode}{Coverage percentage of the layer}
#'   \item{OriginalName}{Original scientific name}
#'   \item{ScientificName}{Correct scientific name}
#'   \item{TaxonGroupCode}{Grouping of species}
#'   \item{PctValue}{Percentage coverage over the whole plot}
#'   \item{RecordingScale}{Scale on which the recordings are written down}
#'   \item{species_number}{List column with matching synoptic species numbers}
#'   ... and other columns from inbodb.
#' }
#' @source Generated sample data from Cydonia database in INBO.
"example_recordings"

#' Example synoptic classification table
#'
#' A dataset containing the necessary info for classification of vegetation
#' in syntaxons.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{syntaxonCode}{Syntaxon identifier}
#'   \item{speciesNumber}{Species identifier (links to species list)}
#'   \item{frequency}{Presence frequency of a species (0-100)}
#'   \item{mean_if_present}{The average cover when present (0-100)}
#' }
#' @source Subset of syntaxons for classification.
"example_synoptics"

#' Example species list
#'
#' A dataset linking species names, GBIF keys, and the internal
#' `speciesNumber` used in the synoptic table.
#'
#' @format A data frame with species matching information:
#' \describe{
#'   \item{speciesName}{Name of species (e.g., from OriginalName)}
#'   \item{scientificName}{Accepted scientific name}
#'   \item{usageKey}{GBIF usageKey}
#'   \item{acceptedUsageKey}{GBIF acceptedUsageKey}
#'   \item{speciesNumber}{Internal species identifier}
#'   ... and other columns.
#' }
#' @source Internal INBO species list.
"example_species_list"
