#Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function that tests a list of columns are present in an antaresData
#' object
#'
#' @param x
#'   Object of class 'antaresData'
#' @param cols
#'   A list. Each element has a name of an element of 'x' ("areas",
#'   "links" "clusters" and/or "districts") containing the needed columns of
#'   that element. For instance cols = list(areas = "LOAD") indicates that the
#'   element "areas" of 'x' needs to have the column "LOAD".
#'
#' @return
#'   It returns x if it is an antaresDataList. Else it returns an
#'   antaresDataList containing "x".
#'
#' @noRd
.checkColumns <- function(x, cols) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  if (is(x, "antaresDataTable")) {
    attrs <- attributes(x)
    x <- list(x)
    names(x) <- attrs$type
    for (a in c("timeStep", "synthesis", "opts")) setattr(x, a, attrs[[a]])
    class(x) <- append(c("antaresDataList", "antaresData"), class(x))
  }

  missingCols <- c()

  for (n in names(cols)) {
    missingCols <- append(missingCols, setdiff(cols[[n]], names(x[[n]])))
  }

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  return(x)
}

.checkAttrs <- function(x, timeStep = NULL, synthesis = NULL, type = NULL) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  attrs <- attributes(x)

  if (!is.null(timeStep) && timeStep != attrs$timeStep) {
    stop("'x' has a ", attrs$timeStep, " time step, but the function requires a",
         timeStep, " time step.")
  }

  if (!is.null(synthesis) && synthesis != attrs$synthesis) {
    if (attrs$synthesis) stop("'x' contains synthetic results but the function requires detailed results")
    else stop("'x' contains detailed results but the function needs synthetic results")
  }

  x
}
