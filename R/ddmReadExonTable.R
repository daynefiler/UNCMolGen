#' @title Read and process SOPHiA exon coverage table
#'
#' @param fpath Character, path(s) to exon coverage table
#' @param ver Character scalar, DDM version
#'
#' @details
#' No modifications. Will add 'DDM_version' field when 'ver' parameter NOT NULL.
#'
#' @return [data.table::data.table] object with exon coverage data
#' @import data.table
#' @importFrom rlang is_scalar_character
#' @export

ddmReadExonTable <- function(fpath, ver = NULL) {

  ## DT bindings:
  DDM_version <- NULL

  stopifnot(is.character(fpath))
  stopifnot(file.exists(fpath))
  stopifnot(is_scalar_character(ver) | is.null(ver))

  ## Read in file
  dat <- lapply(fpath, fread)
  dat <- rbindlist(dat)

  ## TODO: add in checks for appropriate-looking SOPHiA file

  ## Add DDM version
  if (!is.null(ver)) dat[ , DDM_version := ver]

  dat[]

}
