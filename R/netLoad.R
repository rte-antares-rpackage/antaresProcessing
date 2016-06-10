#' Net load of areas
#'
#' This function computes the net load of areas at the desired time step. Net
#' load is the load of an area minus productions that are not controlled:
#' wind, solar, hydraulic run of river, etc. the production of clusters in
#' must run mode is also subtracted by default.
#'
#' @param x
#'   An \code{antaresData} object created with readAntares. unless
#'   \code{ignoreMustRun} is true, it must
#'   have a column \code{mustRunTotal}.
#' @param ignoreMustRun
#'   If \code{TRUE}, the production in must run mode is not substracted to the
#'   net load.
#' @inheritParams surplus
#'
#' @return
#' A data.table of class \code{antaresDataTable}. It contains one line per
#' area, timeId and scenario and has the following columns:
#' \item{area}{Name of the area}
#' \item{timeId}{time id and other time columns}
#' \item{mcYear}{id of the Monte-Carlo scenario}
#' \item{netLoad}{Net Load}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", synthesis = FALSE, mustRun= TRUE)
#'
#' netLoad(mydata)
#' netLoad(mydata, "monthly")
#'
#' }
#'
#' @export
#'
netLoad <- function(x, timeStep = "hourly", ignoreMustRun = FALSE) {

  x <- .checkColumns(x, list(areas = c("LOAD", "ROW BAL.", "PSP", "MISC. NDG",
                                       "H. ROR", "WIND", "SOLAR")))

  idVars <- .idCols(x$areas)

  if (ignoreMustRun) {

    res <- x$areas[, append(mget(idVars),
                            .(netLoad = LOAD - `ROW BAL.` - PSP - `MISC. NDG` -
                                `H. ROR` - WIND - SOLAR))]

  } else {

    x <- .checkColumns(x, list(areas = "mustRunTotal"))

    res <- x$areas[, append(mget(idVars),
                            .(netLoad = LOAD - `ROW BAL.` - PSP - `MISC. NDG` -
                                `H. ROR` - WIND - SOLAR - mustRunTotal))]
  }



  res <- .setAttrs(res, "netLoad", simOptions(x))

  changeTimeStep(res, timeStep)
}
