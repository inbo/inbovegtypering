#' Load synoptic data
#'
#' Synoptic data are used as basis for the classification
#'
#' @param source location where to find the data
#' @importFrom readr read_csv2 locale
#' @return tibble with synoptic data
#' @export
#'
load_synoptic_data <- function(source = "manual", path = NULL) {
  if (source == "test") {
    synoptic_data <-
      read_csv2("inst/resources/synoptic_tabel.csv",
        locale = locale(decimal_mark = ",", grouping_mark = "."),
        show_col_types = FALSE
      )
  } else if (source == "package") {
    synoptic_data <- read_csv2(
      file.path(
        system.file(package = "inbovegtypering"),
        "resources",
        "synoptic_tabel.csv"
      ),
      locale(decimal_mark = ",", grouping_mark = "."),
      show_col_types = FALSE
    )
  } else if (source == "manual") {
    if (is.null(path)) {
      stop("when source is manual, the path is mandatory")
    }
    synoptic_data <-
      read_csv2(path,
        locale = locale(decimal_mark = ",", grouping_mark = "."),
        show_col_types = FALSE
      )
  } else {
    stop("No valid source selected for the synoptic data")
  }
  synoptic_data
}
