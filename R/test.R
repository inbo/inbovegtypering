#' Test function
#'
#' @param name name of the user
#'
#' @return print statement
#' @importFrom glue glue
#' @export
#'
#' @examples
#' test("Pieter")
test <- function(name) {
  glue::glue("Hello, {name}")
}
