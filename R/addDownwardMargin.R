# Copyright © 2016 RTE Réseau de transport d’électricité
setAlias(
  "downwardMargin",
  "Data required by 'addDownwardMargin()'",
  c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE",
    "ROW BAL.", "linkCapacity", "mustRun")
)

#'Add downward margins of areas
#'
#' This function computes isolated and interconnected downward margins of areas and
#' add them to an antaresData object.
#'
#' @inheritParams addUpwardMargin
#'
#' @return
#' The function modifies its input by adding to it two new columns
#' \code{isolatedDownwardMargin} and \code{interconnectedDownwardMargin}. For
#' convenience it invisibly returns \code{x}.
#'
#' @details
#' For a given area, downward margin is equal to the thermal minimum production
#' (due must run production and minimum stable power of production units) plus
#' the fatal productions minus the load and the pumping capacity. More formally
#' it is equal to:
#'
#' \code{isolatedDownwardMargin = thermalPMin + `H. ROR` + WIND + SOLAR + `MISC. NDG`
#'                                - LOAD - pumpingCapacity}
#'
#' The variable \code{pumpingCapacity} is automatically created when pumped
#' storage areas are removed with function
#' \code{\link[antaresRead]{removeVirtualAreas}}. If there is not any such area,
#' \code{pumpingCapacity} is assumed to be equal to 0.
#'
#' Interconnected downward margin is the isolated downward margin plus the exports
#' minus the imports:
#'
#' \code{interconnectedDownwardMargin = isolatedDownwardMargin + BALANCE - `ROW BAL.`}
#'
#' @examples
#' \dontrun{
#' # data required by the function
#' showAliases("downwardMargin")
#'
#' mydata <- readAntares(select = "downwardMargin")
#' mydata <- removeVirtualAreas(mydata, getAreas(c("pump", "stor")))
#'
#' addDownwardMargin(mydata)
#' names(mydata$areas)
#' }
#' @export
addDownwardMargin <- function(x) {
  if (!is(x, "antaresData")) stop("'x' needs to be an 'antaresData' object created with readAntares")
  if (is(x, "antaresDataList")) {
    if (is.null(x$areas) & is.null(x$districts)) {
      warning("'x' does not contain area or district data. Upward margin has not been computed.")
    } else {
      if (!is.null(x$areas)) {
        setattr(x$areas, "virtualNodes", attr(x, "virtualNodes"))
        addDownwardMargin(x$areas)
      }
      if (!is.null(x$districts)) addDownwardMargin(x$districts)
    }
    return(invisible(x))
  }

  # Check required columns are present
  .neededColAreaDown <- c("H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "ROW BAL.")
  .checkColumns(x, list(areas = .neededColAreaDown))

  # If the study contains storage/capacity areas, the column 'pumpingCapacity' is
  # also required
  if(!is.null(attr(x, "virtualNodes")) &&
     !is.null(attr(x, "virtualNodes")$storageFlexibility)) {
    if (is.null(x$pumpingCapacity)) {
      stop("Storage/flexibility areas have been declared but column ",
           "'storageCapacity'is missing. Use 'linkCapacity = TRUE' in ",
           "readAntares() and then use removeVirtualAreas().")
    }
  }

  if (is.null(x$pumpingCapacity)) pumpCap <- 0
  else pumpCap <- x$pumpingCapacity

  # Check thermalPmin is here
  if (is.null(x$thermalPmin)) {
    stop("Column 'thermalPmin' is missing. Use option 'mustRun = TRUE' in readAntares() to add it")
  }

  x[, isolatedDownwardMargin := thermalPmin - pumpCap + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD]
  x[, interconnectedDownwardMargin := isolatedDownwardMargin + BALANCE - `ROW BAL.`]
}
