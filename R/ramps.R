#' Ramp of an area
#'
#' This function computes the ramp of the consumption and the balance of an area
#' at a given time step.
#'
#' @param x
#'   Object of class \code{antaresData} containing data for areas. It must
#'   contain the columns needed to compute the net load and the column
#'   \code{BALANCE}.
#' @inheritParams addNetLoad
#'
#' @return
#' A data.table of class \code{antaresDataTable} containing the foloowing columns:
#' \item{area}{Area name.}
#' \item{timeId}{time id and other time columns depending on the time step of the data.}
#' \item{mcYear}{Scenario id. Visible only if it is present in the source data.}
#' \item{netLoadRamp}{Ramp of the net load of an area.}
#' \item{balanceRamp}{Ramp of the balance of an area.}
#' \item{areaRamp}{Sum of the two previous columns.}
#'
#'
#' @examples
#' \dontrun{
#'
#'   mydata <- readAntares(areas = "all", mustRun = TRUE)
#'   ramps(mydata)
#' }
#'
#' @export
#'
ramps <- function(x, ignoreMustRun = FALSE) {
  x <- .checkColumns(x, list(areas = c("BALANCE")))

  netLoad <- netLoad(x, ignoreMustRun = ignoreMustRun)
  netLoad$balance <- x$areas$BALANCE

  if (is.null(netLoad$mcYear)) idVars <- "area"
  else idVars <- c("area", "mcYear")

  setorderv(netLoad, c(idVars, "timeId"))
  netLoad[, `:=`(netLoadRamp = netLoad - shift(netLoad, fill = 0),
                 balanceRamp = balance - shift(balance, fill = 0))]

  netLoad[timeId == min(timeId), c("netLoadRamp", "balanceRamp") := 0]
  netLoad[, areaRamp := netLoadRamp + balanceRamp]

  res <- netLoad[, c(.idCols(netLoad), "netLoadRamp", "balanceRamp", "areaRamp"), with = FALSE]

  res <- .setAttrs(res, "ramps", simOptions(x))

  res
}
