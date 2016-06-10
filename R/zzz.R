#' @import data.table
#' @import antaresRead

.idCols <- antaresRead:::.idCols
.addClassAndAttributes <- antaresRead:::.addClassAndAttributes

.setAttrs <- function(x, type, opts, timeStep = "hourly", synthesis = FALSE) {
  .addClassAndAttributes(x, synthesis, timeStep, opts, type = type)
}
