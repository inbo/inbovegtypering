#' Example Vegetation Recordings
#'
#' A dataset containing vegetation recordings (relevés). This dataset includes
#' detailed taxonomic information (GBIF keys) and cover values transformed to
#' percentages.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{Name}{Character. The name of the recording site or project.}
#'   \item{RecordingGivid}{Character. Unique identifier for the specific recording event.}
#'   \item{UserReference}{Character. User-provided reference code for the recording.}
#'   \item{LayerCode}{Character. Code indicating the vegetation layer (e.g., "K" for herb layer, "S" for shrub layer).}
#'   \item{CoverCode}{Character. The original cover code recorded in the field (e.g., Braun-Blanquet code).}
#'   \item{OriginalName}{Character. The taxon name as originally recorded.}
#'   \item{ScientificName}{Character. The resolved scientific name of the taxon.}
#'   \item{TaxonGroupCode}{Character. Code indicating the taxonomic group (e.g., "Vascular").}
#'   \item{PhenologyCode}{Character. Code indicating the phenological stage.}
#'   \item{Comment}{Character. Field observations or comments.}
#'   \item{CoverageCode}{Character. Standardized coverage code.}
#'   \item{PctValue}{Numeric. The cover value transformed to a percentage (0-100). Used for quantitative distance calculations.}
#'   \item{RecordingScale}{Character. The name of the scale used for recording (e.g., "Londo", "Braun-Blanquet").}
#'   \item{usageKey}{Integer. The GBIF backbone usage key for the taxon.}
#'   \item{acceptedUsageKey}{Integer. The GBIF backbone key for the accepted taxon.}
#'   \item{rank}{Character. The taxonomic rank of the record (e.g., "SPECIES").}
#'   \item{speciesKey}{Integer. GBIF key for the species rank.}
#'   \item{genusKey}{Integer. GBIF key for the genus rank.}
#'   \item{familyKey}{Integer. GBIF key for the family rank.}
#'   \item{orderKey}{Integer. GBIF key for the order rank.}
#'   \item{classKey}{Integer. GBIF key for the class rank.}
#'   \item{phylumKey}{Integer. GBIF key for the phylum rank.}
#'   \item{kingdomKey}{Integer. GBIF key for the kingdom rank.}
#' }
#' @usage data(example_recordings)
"example_recordings"

#' Example Synoptic Table
#'
#' A synoptic table defining vegetation syntaxa (types). It links syntaxon codes
#' to internal species numbers and provides frequency and characteristic cover
#' statistics.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{syntaxonCode}{Character. Unique identifier for the vegetation type (syntaxon).}
#'   \item{speciesNumber}{Integer. Internal numeric identifier for the species, linking to `example_species_list`.}
#'   \item{frequency}{Numeric. The frequency of the species within the syntaxon (percentage 0-100).}
#'   \item{mean_if_present}{Numeric. The mean cover of the species calculated only for plots where it is present (percentage 0-100).}
#'   \item{row_id}{Integer/Character. Unique identifier for the row in the synoptic table.}
#'   \item{usageKey}{Integer. The GBIF backbone usage key for the taxon.}
#'   \item{acceptedUsageKey}{Integer. The GBIF backbone key for the accepted taxon.}
#' }
#' @usage data(example_synoptics)
"example_synoptics"

#' Example Species List
#'
#' A lookup table mapping internal species numbers to scientific names and
#' GBIF taxonomic keys.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{speciesName}{Character. The vernacular or display name of the species.}
#'   \item{scientificName}{Character. The full scientific name.}
#'   \item{usageKey}{Integer. The GBIF backbone usage key.}
#'   \item{acceptedusageKey}{Integer. The GBIF backbone key for the accepted taxon.}
#'   \item{speciesNumber}{Integer. The internal numeric identifier used to link recordings to the synoptic table.}
#' }
#' @usage data(example_species_list)
"example_species_list"
