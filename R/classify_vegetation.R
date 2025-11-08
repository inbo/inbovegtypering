#' Classify vegetation relevés
#'
#' This is the main user-facing function for classifying one or more vegetation
#' relevés using a chosen method from van Tongeren (2008).
#'
#' @param data A data frame of relevé(s) data, prepared by
#'   `read_inboveg_recording()` or in the same format. It must contain
#'   `RecordingGivid`, `species_number`, and `PctValue`.
#' @param synoptics A data frame with the synoptic table, or the string
#'   "default" to use the package's default table. See
#'   `get_synoptic_table()`.
#' @param method A character string specifying the classification method.
#'   One of:
#'   \itemize{
#'     \item `"cod"`: Composite Distance (Eq. 9, default)
#'     \item `"likelihood"`: -2ln(Likelihood) (Eq. 4)
#'     \item `"weirdness"`: Weirdness Index (Eq. 5)
#'     \item `"incompleteness"`: Incompleteness Index (Eq. 6)
#'     \item `"med"`: Modified Euclidean Distance (Eq. 8)
#'   }
#' @param ... Additional arguments passed to the specific classification
#'   function (e.g., `r` and `s` for `"cod"`, `t` for `"med"`).
#'
#' @return An object of class `inbovegclassification_list`, containing one
#'   `inbovegclassification` object for each `RecordingGivid`.
#'
#' @export
#' @importFrom dplyr group_by summarise left_join mutate arrange rename
#' @importFrom dplyr filter select across all_of group_split
#' @importFrom tidyr crossing
#' @importFrom purrr map
#' @importFrom rlang .data sym
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_releve' is a correctly formatted data frame
#' # and 'con_taxa' is a connection
#'
#' # 1. Get relevé data
#' con_inboveg <- connect_db_inboveg(test = TRUE)
#' con_taxa <- connect_db_taxonomy(test = TRUE) # Needs setup
#' my_releve <- read_inboveg_recording(con_inboveg, con_taxa,
#'   survey = "ExampleSurvey",
#'   code = "ExampleReleve1"
#' )
#'
#' # 2. Classify using the default method (Composite Distance)
#' classification_cod <- classify_vegetation(my_releve, synoptics = "default")
#'
#' # 3. Classify using -2ln(Likelihood)
#' classification_ll <- classify_vegetation(my_releve,
#'   synoptics = "default",
#'   method = "likelihood"
#' )
#'
#' # 4. Classify using Modified Euclidean Distance with custom 't'
#' classification_med <- classify_vegetation(my_releve,
#'   synoptics = "default",
#'   method = "med", t = 0.5
#' )
#'
#' # Print results
#' print(classification_cod)
#' }
classify_vegetation <- function(data, synoptics = "default",
                                method = c(
                                  "cod", "likelihood", "weirdness",
                                  "incompleteness", "med"
                                ), ...) {
  # Validate method
  method <- match.arg(method)

  # Load the synoptic table
  synoptic_table <- get_synoptic_table(synoptics)

  # Prepare the base data for analysis
  analysis_df <- create_analysis_data(data, synoptic_table)

  # Call the appropriate classification function
  result_list <- switch(method,
    cod = classify_cod(analysis_df, ...),
    likelihood = classify_likelihood(analysis_df, ...),
    weirdness = classify_weirdness(analysis_df, ...),
    incompleteness = classify_incompleteness(analysis_df, ...),
    med = classify_med(analysis_df, ...)
  )

  return(result_list)
}
