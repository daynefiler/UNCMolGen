#' @title Get filepath for report template
#' @description Get filepath for report template
#'
#' @param name Character scalar, the report name
#'
#' @importFrom rlang is_scalar_character
#' @export

getTemplate <- function(name) {
  stopifnot(is_scalar_character(name))
  name <- sub("[:.:]rmd$", "", name, ignore.case = TRUE)
  name <- sprintf("%s.Rmd", name)
  system.file(file.path("report-templates", name), package = "UNCMolGen")
}
