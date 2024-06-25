#Copyright © 2016 RTE Réseau de transport d’électricité

#' Compare two simulations or two antaresData
#'
#' \code{compare} has been designed to compare two surpluses created with function
#' "surplus" but it can be used to compare the values of two tables of
#' class \code{antaresData} that contain the same type of data.
#'
#' @param x
#'   Table of class \code{antaresData}. x can be an antaresDataTable or antaresDataList.
#' @param y
#'   Table of class \code{antaresData}. x can be an antaresDataTable or antaresDataList.
#'   It must contain the same type of data than 'x': if 'x' contains areas,
#'   it must contain areas, ... Moreover it has to have same time step and
#'   contain either synthetic or detailed results like 'x'.
#' @param method
#'   Method used two compare the two tables. \code{"diff"} compute the difference
#'   between 'y' and 'x'. \code{"ratio"} computes the ratio between 'y' and 'x'.
#'   Finally, \code{"rate"} computes the rate of change between 'y' and 'x' (
#'   it is equal to the ratio between 'y' and 'x' minus one).
#'
#' @return
#' a data.table of class \code{antaresDataTable}. It contains all shared rows and
#' columns between 'x' and 'y'. The columns contain the statistic chosen:
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
#'
#' opts1 <- setSimulationPath(studyPath,-1)
#' mydata1<-readAntares(areas = "all",
#' links = "all",
#' select = c("allAreas", "allLinks"),
#' mcYears = c(1),
#' linkCapacity = TRUE)
#'
#' opts2 <- setSimulationPath(studyPath,-2)
#' mydata2 <- readAntares(areas = "all",
#' links = "all",
#' select = c("allAreas", "allLinks"),
#' mcYears = c(1),
#' linkCapacity = TRUE)
#'
#' opts3 <- setSimulationPath(studyPath,-3)
#' mydata3 <- readAntares(areas = "all",
#' links = "all",
#' select = c("allAreas", "allLinks"),
#' mcYears = c(1),
#' linkCapacity = TRUE)
#'
#' opts4 <- setSimulationPath(studyPath, -4)
#' mydata4 <- readAntares(areas = "all",
#' links = "all",
#' select=c("allAreas", "allLinks"),
#' mcYears = c(1),
#' linkCapacity = TRUE)
#'
#' opts5 <- setSimulationPath(studyPath, -5)
#' mydata5 <- readAntares(areas = "all",
#' links = "all",
#' select=c("allAreas", "allLinks"),
#' mcYears = c(1),
#' linkCapacity = TRUE)
#'
#' resCompare1 <- compare(mydata2, mydata1, method = "diff")
#' resCompare2 <- compare(mydata3, mydata1, method = "diff")
#' resCompare3 <- compare(mydata4, mydata1, method = "diff")
#' resCompare4 <- compare(mydata5, mydata1, method = "diff")
#'
#' listCompare <- list(resCompare1, resCompare2, resCompare3, resCompare4)
#'
#' for (i in 1:length(listCompare)){
#' listCompare[[i]] <- removeVirtualAreas(listCompare[[i]],
#'                                        storageFlexibility =
#'                                        getAreas(select = c("z_dsr", "y_mul", "pum", "tur")))
#' }
#'
#' ml <- readRDS("path/to/mapLayout.rds")
#' plotMap(listCompare, ml)
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
  # if compare is empty return a warning
  # no rows
  if(dim(res)[1]==0){
    warning("compare is empty, x and y must the same ids. For example, table 'areas' of x and y must contains the same values for 'area', 'mcYear', 'timeId', 'time', 'day', 'month' and 'hour'.")
    return(NULL)
  }else{
    return(res)
  }
}
