#' Compute the modulation of cluster units
#'
#' This function computes the modulation of cluster units or of sectors.
#'
#' @param x
#'   An \code{antaresData} object created with \code{readAntares}. It must
#'   contain the hourly detailed results for clusters if \code{by = "cluster"}
#'   or for areas and/or districts if \code{by = "sector"}
#' @param by
#'   Should modulations computed by cluster or by sector ? Possible values are
#'   "sector" and "cluster".
#' @inheritParams surplus
#' @inheritParams surplusClusters
#'
#' @return
#' A data.table of class \code{antaresDataTable} or a list of such tables with
#' the following columns:
#' \item{area}{
#'   Area name. If \code{byDistrict=TRUE}, this column is replaced by column
#'   \code{district}.
#' }
#' \item{cluster}{
#'   Cluster name. If \code{by="sector"}, this column is replaced by column
#'   \code{sector}.
#' }
#' \item{timeId}{
#'   Time id and other time columns.
#' }
#' \item{meanUpWardModulation}{
#'   Average upward modulcation of a cluster unit or of the sector.
#' }
#' \item{meanDownWardModulation}{
#'   Average downward modulation of a cluster unit or of the sector.
#' }
#' \item{meanAbsoluteModulation}{
#'   Average absolute modulation of a cluster unit or of the sector.
#' }
#' \item{maxUpWardModulation}{
#'   Maximal upward modulation of a cluster unit or of the sector.
#' }
#' \item{maxDownWardModulation}{
#'   Maximal downward modulation of a cluster unit or of the sector.
#' }
#' \item{maxAbsoluteModulation}{
#'   Maximal absolute modulation of a cluster unit or of the sector.
#' }
#'
#' Notice that if \code{by="cluster"}, the function computes the modulation per
#' unit, ie. the modulation of a cluster divided by the number of units of the
#' cluster. On the opposite, if \code{by="sector"}, the function returns the
#' modulation of the global production of the sector. Moreover, if parameter
#' \code{x} contains area and district data, the function returns a list with
#' components \code{areas} and \code{districts}.
#'
#' @examples
#' \dontrun{
#'
#' # Modulation of cluster units
#' mydata <- readAntares(clusters = "all", synthesis = FALSE)
#' modulation(mydata)
#'
#' # Aggregate Monte-Carlo scenarios
#' modulation(mydata, synthesis = TRUE)
#'
#' # Modulation of sectors
#' mydata <- readAntares(areas = "all", synthesis = FALSE)
#' modulation(mydata, by = "sector")
#'
#' # Modulation of sectors per district
#' mydata <- readAntares(districts = "all", synthesis = FALSE)
#' modulation(mydata, by = "sector")
#'
#' # Example that minimizes the quantity of data read for getting the modulation
#' # per area
#' mydata <- readAntares(areas = "all", synthesis = FALSE,
#'                       select = c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL",
#'                                  "MIX. FUEL", "MISC. DTG", "H. STOR", "H. ROR",
#'                                  "SOLAR", "WIND")) # Any subset is accepted
#' modulation(mydata, by = "sector")
#' }
#'
#' @export
#'
modulation <- function(x, timeStep = "annual", synthesis = FALSE,
                       by = c("cluster", "sector"), clusterDesc = NULL) {
  by <- match.arg(by)

  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)
  opts <- simOptions(x)

  # The code below does the following:
  #
  # - If by = cluster, use cluster data to compute modulations
  # - If by = sector:
  #   - If x is an antaresDataTable return a data.table with sector modulations
  #   - Else if x is an antaresDataList:
  #     - error if x has no component areas or districts
  #     - return a data.table if only one component areas or districts
  #     - return a list if it has the two components areas and districts

  if (by == "cluster") {

    x <- .checkColumns(x, list(clusters = "production"))

    # Add variable unitcount to the table containing the production of clusters
    if (is.null(clusterDesc)) clusterDesc <- readClusterDesc(opts)
    tmp <- merge(x$clusters,
                 clusterDesc[, .(area, cluster, unitcount)],
                 by = c("area", "cluster"))

    return(.computeModulation(tmp, timeStep, synthesis, opts))

  } else { # by = "sector"

    if (is(x, "antaresDataTable")) {

      if (! attr(x, "type") %in% c("areas", "districts"))
        stop("x needs to contain area or district data.")

      tmp <- .prodPerSector(x)
      return(.computeModulation(tmp, timeStep, synthesis, opts))

    } else { # 'x' is an antaresDataList

      res <- list()
      if (!is.null(x$areas)) {
        tmp <- .prodPerSector(x$areas)
        res$areas <- .computeModulation(tmp, timeStep, synthesis, opts)
      }
      if (!is.null(x$districts)) {
        tmp <- .prodPerSector(x$districts)
        res$districts <- .computeModulation(tmp, timeStep, synthesis, opts)
      }

      if (length(res) == 0) stop("x needs to contain area and/or district data and at least one sector production (NUCLEAR, LIGNITE, ...).")

      .addClassAndAttributes(res, synthesis, timeStep, opts, simplify = TRUE)
    }
  }
}

.computeModulation <- function(x, timeStep, synthesis, opts) {
  # Compute production at t-1. A bit hacky but saves a lot of time !
  setorderv(x, .idCols(x))
  x[, shiftProd := ifelse(timeId == min(timeId), 0, shift(production))]

  # Modulations
  res <- x[, append(mget(.idCols(x)),
                    .(meanUpwardModulation = pmax(0, production - shiftProd) / unitcount,
                      meanDownwardModulation = pmax(0, shiftProd - production) / unitcount,
                      meanAbsoluteModulation = abs(production - shiftProd) / unitcount))]
  res[, maxUpwardModulation := meanUpwardModulation]
  res[, maxDownwardModulation := meanDownwardModulation]
  res[, maxAbsoluteModulation := meanAbsoluteModulation]

  # Set correct attributes to the result and aggregate
  res <- .addClassAndAttributes(res, FALSE, "hourly", opts, type = "modulations")

  res <- changeTimeStep(res, timeStep, fun = c("mean", "mean", "mean", "max", "max", "max"))

  if (synthesis) res <- .aggregateMcYears(res, fun = c(mean, mean, mean, max, max, max))

  res
}


#' Private function that creates a table with columns area, sector, timeId,
#' mcYear and production. The later column contains the production of the sector
#'
#' @param x
#'   data.table of class antaresDataTable with type "areas" or "districts".
#' @noRd
.prodPerSector <- function(x) {
  sectors <- antaresRead:::pkgEnv$production

  sectors <- intersect(sectors, names(x))
  if (length(sectors) == 0) return(NULL)

  idVars <- .idCols(x)

  res <- melt(x[, c(idVars, sectors), with = FALSE], id.vars = idVars,
              variable.name = "sector", value.name = "production")

  res$unitcount <- 1L

  res
}
