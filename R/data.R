#' Dummy Vegetation Recordings
#'
#' A synthetic dataset containing vegetation recordings (relevés)
#' generated for testing
#' classification functions.
#' It includes 5 recordings with varying species composition.
#' Recordings "REC-01" and "REC-02" are designed to be very similar.
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{recording}{Character.
#'   Unique identifier for the recording event (relevé) (e.g., "REC-01").}
#'   \item{species_number}{Integer.
#'   Internal numeric identifier for the species (1-20).}
#'   \item{pct_value}{Numeric.
#'   The cover percentage of the species (0-100).}
#'   \item{layer_code}{Character.
#'   Code indicating the vegetation layer
#'   (fixed as "K" for herb layer).}
#' }
#' @usage data(dummy_recordings)
#' @export
"dummy_recordings"

#' Dummy Synoptic Table
#'
#' A synthetic synoptic table defining 10 vegetation syntaxa.
#' It provides the frequency
#' and characteristic cover for 20 dummy species within these types.
#' Syntaxa "SYN-01" and "SYN-02" are designed to be similar to recordings
#'  "REC-01" and "REC-02".
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{syntaxon_code}{Character.
#'   Unique identifier for the vegetation type (syntaxon) (e.g., "SYN-01").}
#'   \item{species_number}{Integer.
#'   Internal numeric identifier for the species (1-20).}
#'   \item{frequency}{Numeric.
#'   The frequency of the species within the syntaxon (percentage 0-100).}
#'   \item{mean_if_present}{Numeric.
#'   The mean cover of the species when it is present (percentage 0-100).}
#' }
#' @usage data(dummy_synoptics)
#' @export
"dummy_synoptics"

#' Dummy Species List
#'
#' A reference list linking the internal species numbers used
#' in the dummy recordings and
#' synoptics to synthetic scientific names and GBIF-style keys.
#'
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{speciesNumber}{Integer.
#'   Internal numeric identifier for the species (1-20).}
#'   \item{speciesName}{Character. Vernacular name (e.g., "Species A").}
#'   \item{scientificName}{Character.
#'   Scientific name (e.g., "Genus A species").}
#'   \item{usageKey}{Integer. Synthetic GBIF usage key (1001-1020).}
#'   \item{acceptedusageKey}{Integer.
#'   Synthetic GBIF accepted usage key (1001-1020).}
#' }
#' @usage data(dummy_species_list)
#' @export
"dummy_species_list"
