#Copyright © 2016 RTE Réseau de transport d’électricité

#' Compare the surpluses of two simulations or two antaresData
#'
#' \code{compare} has been designed to compare two surpluses created with function
#' \code{\link{surplus}} but it can be used to compare the values of two tables of
#' class \code{antaresData} that contain the same type of data.
#'
#' @param x
#'   Table of class \code{antaresData}
#' @param y
#'   Table of class \code{antaresData}. It must contain the same type of data
#'   than 'x': if 'x' contains areas, it must contain areas, ... Moreover it has
#'   to have same time step and contain either synthetic or detailed results like
#'   'x'.
#' @param method
#'   Method used two compare the two tables. \code{"diff"} compute the difference
#'   between 'y' and 'x'. \code{"ratio"} computes the ratio between 'y' and 'x'.
#'   Finally, \code{"rate"} computes the rate of change between 'y' and 'x' (
#'   it is equal to the ratio between 'y' and 'x' minus one).
#'
#' @return
#' a data.table of class \code{antaresDataTable}. It contains all shared rows and
#' columns between 'x' and 'y'. The columns contains the statistic choosen:
#' difference, ratio or rate of change.
#'
#' @examples
#' \dontrun{
#' # First simulation
#' studyPath <- "path/to/study/"
#'
#' setSimulationPath(studyPath, 1)
#' mydata1 <- readAntares("all", "all", synthesis = FALSE)
#' surplus1 <- surplus(mydata1, groupByDistrict = TRUE)
#'
#' # Second simulation
#' setSimulationPath(studyPath, 2)
#' mydata2 <- readAntares("all", "all", synthesis = FALSE)
#' surplus2 <- surplus(mydata2, groupByDistrict = TRUE)
#'
#' compare(surplus1, surplus2)
#' }
#'
#' @export
#'
compare <- function(x, y, method=c("diff", "ratio", "rate")) {
  method <- match.arg(method)

  if (!is(x, "antaresData")) stop("'x' must be a data.table of class 'antaresData'")
  if (!is(y, "antaresData")) stop("'y' must be a data.table of class 'antaresData'")

  if (is(x, "antaresDataList")) {
    if (!suppressWarnings(all(names(x) == names(y)))){
      stop(paste0(" x and y must have the same names, names(x) :", paste(names(x), collapse = ", "),
                  " names(y) : ", paste(names(y), collapse = ", ")))
    }else{
      res <- list()
      for (i in names(x)){
        res[[i]] <- compare(x[[i]], y[[i]], method)
      }
      attrs <- attributes(x)
      res <- .addClassAndAttributes(res, synthesis = attrs$synthesis, timeStep = attrs$timeStep,
                             type = paste0(attrs$type, "Comparison"), opts = attrs$opts)

      res <- antaresRead::as.antaresDataList(res)
      return(res)
    }
  }

  # Check that x and y are comparable
  for (t in c("type", "timeStep", "synthesis")) {
    if (attr(x, t) != attr(y, t)) {
      stop("'x' and 'y' are not comparable. 'x' has ", t, " = ", attr(x, t),
           " while 'y' has ", t, " = ", attr(y, t))
    }
  }

  attrs <- attributes(x)

  # Combine the two tables
  sharedVars <- intersect(names(x), names(y))
  sharedIdVars <- intersect(.idCols(x), .idCols(y))
  valueVars <- setdiff(sharedVars, sharedIdVars)

  x <- x[, sharedVars, with = FALSE]
  y <- y[, sharedVars, with = FALSE]
  setnames(y, valueVars, paste0(valueVars, "_y"))

  res <- merge(x, y, by = sharedIdVars)

  if (method == "diff") {
    res[, c(valueVars) :=  res[, paste0(valueVars, "_y"), with = FALSE] - res[, valueVars, with = FALSE]]
  } else if (method == "ratio") {
    res[, c(valueVars) :=  res[, paste0(valueVars, "_y"), with = FALSE] / res[, valueVars, with = FALSE]]
  } else if (method == "rate") {
    res[, c(valueVars) :=  res[, paste0(valueVars, "_y"), with = FALSE] / res[, valueVars, with = FALSE] - 1]
  } else stop("Invalid method")

  res <- res[, sharedVars, with = FALSE]

  #type = paste0(attrs$type, "Comparison")
  res <- .addClassAndAttributes(res, synthesis = attrs$synthesis, timeStep = attrs$timeStep,
                                type = attrs$type, opts = attrs$opts)

  res <- antaresRead::as.antaresDataTable(res)

}
