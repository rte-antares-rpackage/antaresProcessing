#' Add upward margin of areas
#'
#' This function computes isolated and interconnected upward margins of areas and
#' add them to an antaresData object.
#'
#' @param x An object of class \code{antaresData} created with
#'   \code{\link[antaresRead]{readAntares}}
#'
#' @return
#' The function modifies its input by adding to it two new columns
#' \code{isolatedUpwardMargin} and \code{interconnectedUpwardMargin}. For
#' convenience it invisibly returns \code{x}.
#'
#' @details
#' For a given area and time step, isolated upward margin is the difference
#' between the available production capacity plus the fatal productions and the
#' load. More formally it is equal to:
#'
#' \code{isolatedUpwardMargin = (`AVL DTG` + hstorPMaxAvg + storageCapacity) +
#'                              (`H. ROR` + WIND + SOLAR + `MISC. NDG`) - LOAD}
#'
#' The variable \code{storageCapacity} is automatically created when pumped
#' storage areas are removed with function
#' \code{\link[antaresRead]{removeVirtualAreas}}. If there is not any such area,
#' \code{storageCapacity} is assumed to be equal to 0.
#'
#' Interconnected upward margin is the isolated upward margin plus the imports and
#' minus the exports:
#'
#' \code{interconnectedUpwardMargin = isolatedUpwardMargin - BALANCE + `ROW BAL.`}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(
#'   areas = "all", links = "all",
#'   select = c("H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "AVL DTG",
#'              "ROW BAL.", "hydroStorageMaxPower", "FLOW LIN.", "linkCapacity")
#' )
#' mydata <- removeVirtualAreas(mydata, getAreas(c("pump", "stor")))
#'
#' addUpwardMargin(mydata)
#' }
#' @export
#'
addUpwardMargin <- function(x) {
  # Check class of 'x'
  if (!is(x, "antaresData")) stop("'x' needs to be an 'antaresData' object created with readAntares")
  if (is(x, "antaresDataList")) {
    if (is.null(x$areas) & is.null(x$districts)) {
      warning("'x' does not contain area or district data. Upward margin has not been computed.")
    } else {
      if (!is.null(x$areas)) {
        setattr(x$areas, "virtualNodes", attr(x, "virtualNodes"))
        addUpwardMargin(x$areas)
      }
      if (!is.null(x$districts)) addUpwardMargin(x$districts)
    }
    return(invisible(x))
  }

  # Check required columns are present
  .neededColAreaUp<-c("H. ROR", "WIND", "SOLAR", "MISC. NDG",
                      "LOAD", "BALANCE", "ROW BAL.", "AVL DTG")
  .checkColumns(x, list(areas = .neededColAreaUp))

  # If the study contains storage/capacity areas, the column 'storageCapacity' is
  # also required
  if(!is.null(attr(x, "virtualNodes")) &&
     !is.null(attr(x, "virtualNodes")$storageFlexibility)) {
    if (is.null(x$storageCapacity)) {
      stop("Storage/flexibility areas have been declared but column ",
           "'storageCapacity'is missing. Use 'linkCapacity = TRUE' in ",
           "readAntares() and then use removeVirtualAreas().")
    }
  }
  if (is.null(x$storageCapacity)) storCap <- 0
  else storCap <- x$storageCapacity

  # Check that the additional column "hstorPMaxAvg" is present. If not, just
  # throw a warning instead of an because in some studies this variable is not used.
  if (is.null(x$hstorPMaxAvg)) {
    warning("Column 'hstorPMaxAvg' is missing. Upward margins can be wrong unless ",
            "hydraulic storage is not used in your study. To add this column, ",
            "use option 'hydroStorageMaxPower = TRUE' in readAntares().")
    hstorpmax <- 0
  } else hstorpmax <- x$hstorPMaxAvg

  x[, isolatedUpwardMargin := `AVL DTG` + hstorpmax + storCap + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD]
  x[, interconnectedUpwardMargin := isolatedUpwardMargin - BALANCE + `ROW BAL.`]

  invisible(x)
}
