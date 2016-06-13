#' Ramp of an area
#'
#' This function computes the ramp of the consumption and the balance of areas
#' and/or districts and add them to the data.
#'
#' @param x
#'   Object of class \code{antaresData} containing data for areas. it must
#'   contain the column \code{BALANCE} and either the column "netLoad" or
#'   the columns needed to compute the net load.
#' @inheritParams addNetLoad
#'
#' @return
#' \code{addRamps} modify its input by adding to it three new columns:
#' \item{netLoadRamp}{Ramp of the net load of an area.}
#' \item{balanceRamp}{Ramp of the balance of an area.}
#' \item{areaRamp}{Sum of the two previous columns.}
#'
#'For convenience the function invisibly returns the modified input.
#'
#'
#' @examples
#' \dontrun{
#'
#'   mydata <- readAntares(areas = "all", mustRun = TRUE, timeStep = "monthly")
#'
#'   addRamps(mydata)
#' }
#'
#' @export
#'
addRamps <- function(x, ignoreMustRun = FALSE) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  # Ramps are not meaningful for annual data !
  if (attr(x, "timeStep") == "annual") stop("Ramps are not meaningful for annual results")

  if (is(x, "antaresDataList")) {
    if (is.null(x$areas) & is.null(x$districts)) stop("'x' does not contain area or district data")
    if (!is.null(x$areas)) addRamps(x$areas, ignoreMustRun)
    if (!is.null(x$districts)) addRamps(x$districts, ignoreMustRun)
    return(x)
  }

  if(! attr(x, "type") %in% c("areas", "districts")) stop("'x' does not contain area or district data")

  if (!is.null(x$areaRamp)) stop("Ramps columns already exist in input.")
  if (is.null(x$BALANCE)) stop("Column 'BALANCE' is needed but missing.")
  if (is.null(x$netLoad)) addNetLoad(x, ignoreMustRun)

  idVars <- setdiff(.idCols(x), "timeId")

  setorderv(x, c(idVars, "timeId"))
  x[, `:=`(netLoadRamp = netLoad - shift(netLoad, fill = 0),
           balanceRamp = BALANCE - shift(BALANCE, fill = 0))]

  x[timeId == min(timeId), c("netLoadRamp", "balanceRamp") := 0]
  x[, areaRamp := netLoadRamp + balanceRamp]

  invisible(x)
}
