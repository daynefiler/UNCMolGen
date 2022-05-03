#' @title Read and process SOPHiA variant table
#'
#' @param fpath Character, path(s) to variant table
#' @param ver Character scalar, DDM version
#'
#' @details
#' Only modification currently, is removing '%' from var_percent field and
#' coercing to numeric. Will add 'DDM_version' field when 'ver' parameter NOT
#' NULL.
#'
#' @return [data.table::data.table] object with variant data
#' @import data.table
#' @importFrom rlang is_scalar_character
#' @export

ddmReadVariantTable <- function(fpath, ver = NULL) {

  ## DT bindings:
  var_percent <- DDM_version <- NULL

  stopifnot(is.character(fpath))
  stopifnot(file.exists(fpath))
  stopifnot(is_scalar_character(ver) | is.null(ver))

  ## Read in file
  dat <- lapply(fpath, fread)
  dat <- rbindlist(dat)

  ## TODO: add in checks for appropriate-looking SOPHiA file

  ## Reformat var_percent
  dat[ , var_percent := as.numeric(sub("%$", "", var_percent))]

  ## Add DDM version
  if (!is.null(ver)) dat[ , DDM_version := ver]

  dat[]

}
