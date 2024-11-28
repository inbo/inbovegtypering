#' Example vegetation recordings
#'
#' A dataset containing sample vegetation recordings for testing and examples.
#' The data is retrieved from the INBOVEG database Cydonia.
#' The last 5 colums are added for easier use and are created by
#' rgbif::name_backbone_checklist() and some manual curation
#'
#' @format A data frame with 838 rows and 18 variables:
#' \describe{
#'   \item{Name}{Name of the project}
#'   \item{RecordingGivid}{Name of the record}
#'   \item{UserReference}{Naming in the project}
#'   \item{LayerCode}{Layer identifier}
#'   \item{CoverCode}{Coverage percentage of the layer}
#'   \item{OriginalName}{Orignal scheintific na;e}
#'   \item{ScientificName}{Correct scientific name}
#'   \item{TaxonGroupCode}{Grouping of species}
#'   \item{PhenologyCode}{?}
#'   \item{Comment}{Optional comments}
#'   \item{CoverageCode}{Coverage code associated with the recording scale}
#'   \item{PctValue}{Percentage coverage over the whole plog}
#'   \item{RecordingScale}{Scale on which the recordings are written down}
#'   \item{usageKey}{GBIF usageKey, synonyms have different usageKeys}
#'   \item{acceptedUsageKey}{GBIF usageKey for the accepted name for synonyms}
#'   \item{rank}{GBIF matched rank}
#'   \item{speciesKey}{GBIF species key}
#'   \item{genusKey}{GBIF genus key}
#' }
#' @source Generated sample data from Cydonia database in INBO
"example_recordings"



#' Example synoptic classification table
#'
#' A dataset containing the necessary info for classification of vegetation
#' in syntaxons.
#'
#' @format A data frame with 7800 rows and 7 variables (24 syntaxons):
#' \describe{
#'   \item{syntaxonCode}{Recording identifier}
#'   \item{speciesNumber}{Species identifier}
#'   \item{frequency}{Presence frequency of a species in a syntaxon}
#'   \item{mean_if_present}{The average frequency when present in a syntaxon}
#'   \item{row_id}{Rownumber for use when there are several GBIF usageKeys for
#'   the same species }
#'   \item{usageKey}{GBIF usageKey of the species}
#'   \item{acceptedUsageKey}{GBIF accepted usageKey when a synonym}
#' }
#' @source Subset of syntaxons that can be classified based on vegetation
"example_synoptics"
