#' @import data.table
#' @import antaresRead

.idCols <- antaresRead:::.idCols
.addClassAndAttributes <- antaresRead:::.addClassAndAttributes

.setAttrs <- function(x, type, opts, timeStep = "hourly", synthesis = FALSE) {
  .addClassAndAttributes(x, synthesis, timeStep, opts, type = type)
}

.aggregateMcYears <- function(x, fun = c(mean)) {
  attrs <- attributes(x)

  idVars <- setdiff(.idCols(x), "mcYear")
  x[, mcYear := NULL]

  if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))

  x <- x[, mapply(function(dt, f) {f(dt)}, dt = .SD, f = fun, SIMPLIFY=FALSE),
         by = idVars]

  #reset attributes
  setattr(x, "type", attrs$type)
  setattr(x, "timeStep", attrs$timeStep)
  setattr(x, "synthesis", FALSE)
  setattr(x, "class", attrs$class)
  setattr(x, "opts", attrs$opts)

  x
}
