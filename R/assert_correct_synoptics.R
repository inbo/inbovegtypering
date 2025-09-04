#' Check correct synoptics format
#'
#' @param object that needs to be assessed
#' @return stop message or data.frame
#'
#' @examples
#' test <- data.frame(syntaxonCode = rep("000", 2),
#'                    speciesNumber = c(437, 463),
#'                    frequency = c(1.1, 3.9),
#'                    mean_if_present = c(2.43, 2.21))
#' assert_correct_synoptics(test)
#' assert_correct_synoptics(airquality)
#'
assert_correct_synoptics <- function(x) {
  # Get reference format from package namespace
  reference_synoptics <-
    utils::getFromNamespace("synoptic_table", "inbovegtypering")
  required_cols <- names(reference_synoptics)

  # Check input type
  if (is.character(x)) {
    if (identical(x[1], "default")) {
      return(reference_synoptics)
    }
    stop("Invalid synoptics argument. Use 'default' or provide a data frame")
  }

  # Validate data frame
  if (!is.data.frame(x)) {
    stop("Synoptics must be either 'default' or a data frame")
  }

  # Check columns
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Synoptics is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Check data types of critical columns
  expected_types <- list(
    speciesNumber = is.numeric,
    syntaxonCode = is.character,
    frequency = is.numeric,
    mean_if_present = is.numeric
  )

  for (col in names(expected_types)) {
    if (!expected_types[[col]](x[[col]])) {
      stop(sprintf(
        "Column '%s' has incorrect data type. Expected: %s",
        col,
        sub("^is\\.", "", deparse(substitute(expected_types[[col]])))
      ))
    }
  }

  # Check value ranges if applicable
  if (any(x$frequency < 0 | x$frequency > 100, na.rm = TRUE)) {
    stop("Frequency values must be between 0 and 100")
  }

  return(x)
}
