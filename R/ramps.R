#Copyright © 2016 RTE Réseau de transport d’électricité

#' Ramp of an area
#'
#' This function computes the ramp of the consumption and the balance of areas
#' and/or districts and add them to the data.
#'
#' @param x
#'   Object of class \code{antaresData} containing data for areas and/or
#'   districts. It must contain the column \code{BALANCE}  and either the column
#'   "netLoad" or the columns needed to compute the net load.
#' @param ignoreMustRun
#'   Should the must run production be ignored in the computation of the net
#'   load?
#' @inheritParams surplus
#'
#' @return
#' \code{addRamps} returns a data.table or a list of data.tables with the
#' following columns:
#' \item{netLoadRamp}{Ramp of the net load of an area. If \code{timeStep} is not hourly, then these columns contain the average value for the given time step.\cr
#'                      formula = netLoad - shift(netLoad, fill = 0) }
#' \item{balanceRamp}{Ramp of the balance of an area. If \code{timeStep} is not hourly, then these columns contain the average value for the given time step.\cr
#'                      formula = BALANCE - shift(BALANCE, fill = 0) }
#' \item{areaRamp}{Sum of the two previous columns. If \code{timeStep} is not hourly, then these columns contain the average value for the given time step.\cr
#'                      formula = netLoadRamp + balanceRamp }
#' \item{minNetLoadRamp}{Minimum ramp of the net load of an area.}
#' \item{minBalanceRamp}{Minimum ramp of the balance of an area.}
#' \item{minAreaRamp}{Minimum ramp sum of the sum of balance and net load.}
#' \item{maxNetLoadRamp}{Maximum ramp of the net load of an area.}
#' \item{maxBalanceRamp}{Maximum ramp of the balance of an area.}
#' \item{maxAreaRamp}{Maximum ramp of the sum of balance and net load.}
#'
#' For convenience the function invisibly returns the modified input.
#'
#'
#' @examples
#' \dontrun{
#'
#'   mydata <- readAntares(areas = "all", mustRun = TRUE, synthesis = FALSE)
#'   addRamps(mydata, timeStep = "annual")
#'
#'   # Example that minimizes the quantity of data read
#'   mydata <- readAntares(areas = "all", synthesis = FALSE,
#'                         select = c("LOAD", "ROW BAL.", "PSP", "MISC. NDG",
#'                                    "H. ROR", "WIND", "SOLAR", "BALANCE"))
#'   netLoadRamp(mydata, timeStep = "annual", ignoreMustRun = TRUE)
#' }
#'
#' @export
#'
netLoadRamp <- function(x, timeStep = "hourly", synthesis = FALSE, ignoreMustRun = FALSE) {
  .checkAttrs(x, "hourly", "FALSE")
  opts <- simOptions(x)

  if (is(x, "antaresDataList")) {
    if (is.null(x$areas) & is.null(x$districts)) stop("'x' does not contain area or district data")

    res <- list()

    if (!is.null(x$areas)) res$areas <- netLoadRamp(x$areas, timeStep, synthesis, ignoreMustRun)
    if (!is.null(x$districts)) res$districts <- netLoadRamp(x$districts, timeStep, synthesis, ignoreMustRun)

    if (length(res) == 0) stop("'x' needs to contain area and/or district data.")

    res <- .addClassAndAttributes(res, synthesis, timeStep, opts, simplify = TRUE)

    return(res)
  }

  if(! attr(x, "type") %in% c("areas", "districts")) stop("'x' does not contain area or district data")

  if (is.null(x$BALANCE)) stop("Column 'BALANCE' is needed but missing.")
  if (is.null(x$netLoad)) addNetLoad(x, ignoreMustRun)

  x <- x[, c(.idCols(x), "BALANCE", "netLoad"), with = FALSE]

  idVars <- .idCols(x)

  setorderv(x, idVars)
  x[, `:=`(netLoadRamp = netLoad - shift(netLoad, fill = 0),
           balanceRamp = BALANCE - shift(BALANCE, fill = 0))]

  x[timeId == min(timeId), c("netLoadRamp", "balanceRamp") := 0]
  x[, areaRamp := netLoadRamp + balanceRamp]

  x <- x[, c(idVars, "netLoadRamp", "balanceRamp", "areaRamp"), with = FALSE]

  x <- .addClassAndAttributes(x, FALSE, "hourly", opts, type = "netLoadRamp")

  if (synthesis) {

    x <- synthesize(x, "min", "max", prefixForMeans = "avg")

    x <- changeTimeStep(x, timeStep,
                        fun = c("mean", "min", "max",
                                "mean", "min", "max",
                                "mean", "min", "max"))

  } else if (timeStep != "hourly") {

    x[, `:=`(
      min_netLoadRamp = netLoadRamp,
      min_balanceRamp = balanceRamp,
      min_areaRamp = areaRamp,
      max_netLoadRamp = netLoadRamp,
      max_balanceRamp = balanceRamp,
      max_areaRamp = areaRamp
    )]

    x <- changeTimeStep(x, timeStep,
                        fun = c("mean", "mean", "mean",
                                "min", "min", "min",
                                "max", "max", "max"))

    setcolorder(x, c(.idCols(x),
                     "netLoadRamp", "min_netLoadRamp", "max_netLoadRamp",
                     "balanceRamp", "min_balanceRamp", "max_balanceRamp",
                     "areaRamp", "min_areaRamp", "max_areaRamp"))

    setnames(x,
             c("netLoadRamp", "balanceRamp", "areaRamp"),
             c("avg_netLoadRamp", "avg_balanceRamp", "avg_areaRamp"))
  }

  x
}
