#' @title Summarize the differences between two tables
#' @description Summarize the differences between two tables
#' @param d0 [data.table::data.table] object, passed to [arsenal::comparedf] 'x'
#' @param d1 [data.table::data.table] object, passed to [arsenal::comparedf] 'y'
#' @param mrgFlds Character, passed to [arsenal::comparedf] 'by'
#' @param d0name,d1name Character scalar, names for the objects to appear in
#' tables
#' @param gcol Color indicating no changes
#' @param bcol Color indicating changes present
#'
#' @importFrom arsenal comparedf
#' @importFrom reactable reactable colDef
#' @import data.table
#' @export

tableDiffs <- function(d0, d1, mrgFlds,
                       d0name = "old",
                       d1name = "new",
                       gcol = getOption("UNCMolGen.gcol", GOODCOL),
                       bcol = getOption("UNCMolGen.gcol", BADCOL)) {

  ## DT bindings:
  J <- statistic <- value <- NULL

  diffObj <- arsenal::comparedf(x = d0, y = d1, by = mrgFlds)
  diffSmry <- summary(diffObj)
  diffSmry <- lapply(diffSmry, as.data.table)
  compSmry <- diffSmry$comparison.summary.table
  setkey(compSmry, statistic)
  compStats <- c("Number of variables in x but not y",
                 "Number of variables in y but not x",
                 "Number of variables compared with some values unequal",
                 "Number of observations in x but not y",
                 "Number of observations in y but not x",
                 "Number of observations with some compared variables unequal",
                 "Number of values unequal")
  compSmry <- compSmry[J(compStats)]
  newStats <- c("Variables in x but not y",
                "Variables in y but not x",
                "Variables compared with some values unequal",
                "Observations in x but not y",
                "Observations in y but not x",
                "Observations with some compared variables unequal",
                "Total values unequal")
  compSmry[ , statistic := newStats]
  unqVar <- compSmry[statistic == "Variables in x but not y", value > 0] |
    compSmry[statistic == "Variables in y but not x", value > 0]
  dltVar <- compSmry[statistic == "Variables compared with some values unequal",
                     value > 0]
  unqObs <- compSmry[statistic == "Observations in x but not y", value > 0] |
    compSmry[statistic == "Observations in y but not x", value > 0]
  dltObs <-
    compSmry[statistic == "Observations with some compared variables unequal",
             value > 0]

  compSmry[ , statistic := gsub(" x", paste0(" ", toupper(d0name)), statistic)]
  compSmry[ , statistic := gsub(" y", paste0(" ", toupper(d1name)), statistic)]
  ctbl <- reactable(data = compSmry,
                    rowStyle = function(i) {
                      list(background = ifelse(compSmry[i, "value"] == 0,
                                               gcol,
                                               bcol),
                           fontWeight = "bold",
                           color = "white")
                    },
                    columns = list(
                      statistic = colDef(name = ""),
                      value = colDef(name = "")
                    ))

  list(diffSmry = diffSmry,
       unqVar = unqVar,
       dltVar = dltVar,
       unqObs = unqObs,
       dltObs = dltObs,
       ctbl = ctbl)

}
