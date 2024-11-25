#' Plot Method for inbovegclassification Objects
#'
#' @description
#' Generic plot method that dispatches to appropriate plotting function
#' based on indextype
#'
#' @param x An object of class "inbovegclassification"
#' @param n_types Number of types to show (default 12)
#' @param ... Additional arguments passed to methods
#'
#' @return A ggplot object
#' @examples
#' \dontrun{
#' results <- classify_likelihood(data, synoptics)
#' plot(results)
#' plot(results, index = "likelihood") # likelihood plot
#' }
#' @export
plot.inbovegclassification <- function(x, n_types = 12, ...) {
  # Get indextype from attribute
  indextype <- attr(x, "indextype")
  if (is.null(indextype)) {
    stop("Object missing 'indextype' attribute")
  }

  # Call appropriate plotting function based on indextype
  plot_classification_rose(x, indextype = indextype, n_types = n_types, ...)
}
