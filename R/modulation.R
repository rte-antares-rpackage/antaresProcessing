#' Compute the modulation of cluster units
#'
#' This function computes the modulation of cluster units.
#'
#' @param x
#'   An \code{antaresData} object created with \code{readAntares}. It must
#'   contain the hourly detailed results for clusters
#' @param timeStep
#'   Desired time step for the result
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{cluster}{Cluster name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{meanUpWardModulation}{...}
#' \item{meanDownWardModulation}{...}
#' \item{meanAbsoluteModulation}{...}
#' \item{maxUpWardModulation}{...}
#' \item{maxDownWardModulation}{...}
#' \item{maxAbsoluteModulation}{...}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(clusters = "all")
#'
#' modulation(mydata)
#'
#' }
#'
#' @export
#'
modulation <- function(x, timeStep = "annual") {
  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)
  x <- .checkColumns(x, list(clusters = "production"))
  opts <- simOptions(x)

  clusterDesc <- readClusterDesc(opts)

  tmp <- merge(x$clusters,
               clusterDesc[, .(area, cluster, unitcount)],
               by = c("area", "cluster"))

  setorderv(tmp, .idCols(tmp))

  tmp[, shiftProd := ifelse(timeId == 1, 0, shift(production, fill = 0))]

  res <- tmp[, append(mget(.idCols(tmp)),
                      .(meanUpwardModulation = pmax(0, production - shiftProd) / unitcount,
                        meanDownwardModulation = pmax(0, shiftProd - production) / unitcount,
                        meanAbsoluteModulation = abs(production - shiftProd) / unitcount))]
  res[, maxUpwardModulation := meanUpwardModulation]
  res[, maxDownwardModulation := meanDownwardModulation]
  res[, maxAbsoluteModulation := meanAbsoluteModulation]

  # Set correct attributes to the result
  class(res) <- c("antaresDataTable", "antaresData", "data.table", "data.frame")
  attr(res, "timeStep") <- "hourly"
  attr(res, "synthesis") <- FALSE
  attr(res, "opts") <- opts
  attr(res, "type") <- "modulation"

  changeTimeStep(res, timeStep, fun = c("mean", "mean", "mean", "max", "max", "max"))

}
