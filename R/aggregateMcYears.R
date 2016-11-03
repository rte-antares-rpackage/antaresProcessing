#Copyright © 2016 RTE Réseau de transport d’électricité

#' Aggregate Monte-Carlo scenarios of an antaresDataTable
#'
#' @param x
#'   'antaresDataTable' with a column 'mcYear'
#' @param fun
#'   vector of functions with size equal to the number of columns to aggregate
#'   (number of columns - number of id columns). If it is of length 1, the
#'   function is used for all columns.
#'
#' @return
#' an 'antaresDataTable' with aggregated data. It contains the same columns than
#' the input but without column "mcYears".
#'
#' @note
#' This function is used in functions such as surplus, where the input data needs
#' to contain detailed results but the user may want a synthetic output
#'
#' @noRd
#'
.aggregateMcYears <- function(x, fun = c(mean)) {
  attrs <- attributes(x)

  idVars <- setdiff(.idCols(x), "mcYear")
  x[, mcYear := NULL]

  if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))

  x <- x[, mapply(function(dt, f) {f(dt)}, dt = .SD, f = fun, SIMPLIFY=FALSE),
         by = idVars]

  #reset attributes
  .addClassAndAttributes(x, TRUE, attrs$timeStep, attrs$opts, type = attrs$type)
}
