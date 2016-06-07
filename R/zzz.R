#' @import data.table

.idCols <- antaresRead:::.idCols

.setAttrs <- function(x, type, opts, timeStep = "hourly", synthesis = FALSE) {
  class(x) <- c("antaresDataTable", "antaresData", "data.table", "data.frame")
  attr(x, "timeStep") <- timeStep
  attr(x, "synthesis") <- synthesis
  attr(x, "opts") <- opts
  attr(x, "type") <- type

  x
}
