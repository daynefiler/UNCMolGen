#' @title Boxplot of value differences from two tables
#' @description Draw a boxplot of value differences from the merge of two tables
#'
#' @param d0 [data.table::data.table] object, the first table
#' @param d1 [data.table::data.table] object, the second table
#' @param mrg Character, the list of fields to merge the two data.table objects
#' @param fld Character scalar, the field to plot
#' @param grpFld Character scalar, the field to group by
#' @param gcol Color for boxes WITHOUT changes in fld from d0 to d1
#' @param bcol Color for boxes WITH changes in fld from d0 to d1
#'
#' @import data.table
#' @importFrom graphics boxplot axis title text grconvertY
#' @export

valDiffBoxplot <- function(d0, d1, mrg, fld,
                           grpFld = "sampleID",
                           gcol = getOption("UNCMolGen.gcol", GOODCOL),
                           bcol = getOption("UNCMolGen.gcol", BADCOL)) {

  ## DT bindings:
  x <- y <- dlt <- . <- NULL

  tmp <- merge(d0[ , .SD, .SDcols = c(mrg, fld = fld)],
               d1[ , .SD, .SDcols = c(mrg, fld = fld)],
               by = mrg)
  setnames(tmp, paste(fld, c("x", "y"), sep = "."), c("x", "y"))
  tmp[ , dlt := x - y]
  smy <- tmp[ , .(col = ifelse(any(abs(dlt) > 0), bcol, gcol)), by = c(grpFld)]
  boxplot(dlt ~ get(grpFld),
          tmp,
          frame = FALSE,
          axes = FALSE,
          ann = FALSE,
          border = smy$col)
  axis(side = 2)
  title(ylab = bquote(Delta~.(fld)))
  text(x = 1:11 - 0.25,
       y = grconvertY(0, "npc", "user"),
       labels = smy[ , get(grpFld)],
       col = smy$col,
       pch = 16,
       srt = 90,
       adj = c(0),
       font = 2)
}
