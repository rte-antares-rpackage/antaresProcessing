#' Compute the surplus of clusters
#'
#' This function computes the surplus of clusters of interest. The surplus of a
#' cluster is equal to its production times the marginal cost of the area it
#' belongs to minus variable, fixed and startup costs.
#'
#' @param x
#'   An \code{antaresData} object created with \code{readAntares}. It must
#'   contain an element \code{clusters} and an element \code{areas} with at
#'   least the column \code{MRG. PRICE}.
#' @param surplusLastUnit
#'   Should the surplus of the last unit of a cluster be computed ? If
#'   \code{TRUE}, then \code{x} must have been created with the option
#'   \code{thermalAvailabilities=TRUE} in order to contain the required column
#'   "available units"
#' @param clusterDesc
#'    A table created with the function \code{\link[antaresRead]{readClusterDesc}}.
#'    If is this parameter is set to \code{NULL} (the default), then the function
#'    attemps to read the needed data in the same study as \code{x}.
#' @inheritParams surplus
#' @inheritParams surplusClusters
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{cluster}{Cluster name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{variableCost}{
#'   Proportional costs of production of the cluster\cr
#'   Formula = marginal cost * production
#' }
#' \item{fixedCost}{
#'   Fixed costs of production of the cluster\cr
#'   Formula = NODU * fixed cost
#' }
#' \item{startupCost}{
#'   Start up costs of the cluster.
#' }
#' \item{surplusPerUnit}{
#'   Average surplus per unit of the cluster.\cr
#'   formula = (`MRG. PRICE` * production - opCost - startupCost) / unitcount
#' }
#' \item{surplusLastUnit}{
#'   Surplus of the last unit of the cluster.\cr
#'   formula = (`MRG. PRICE` * prodLastUnit - opCost / pmax(1, NODU) - startup.cost)
#' }
#' \item{totalSurplus}{
#'   Surplus of all units of the cluster.\cr
#'   formula = `MRG. PRICE` * production - opCost - startupCost
#' }
#' \item{economicGradient}{
#'   Economic gradient of a cluster. It is equal to
#'   the surplus per unit divided by the capacity of a unit.\cr
#'   formula = surplusPerUnit / nominalcapacity
#' }
#'
#' @examples
#' \dontrun{
#'
#' mydata <- readAntares(areas = "all", clusters = "all", select = "MRG. PRICE",
#'                       synthesis = FALSE)
#' surplusClusters(mydata)
#'
#' # Computing the surplus of the last unit of a cluster requires the additional
#' # column "availableUnits". To add this column, one has to use parameter
#' # "thermalAvailabilities = TRUE" in readAntares.
#'
#' mydata <- readAntares(areas = "all", clusters = "all", select = "MRG. PRICE",
#'                       thermalAvailabilities = TRUE, synthesis = FALSE)
#' surplusClusters(mydata, surplusLastUnit = TRUE)
#'
#' }
#'
#' @export
#'
surplusClusters <- function(x, timeStep="annual", synthesis = FALSE,
                            surplusLastUnit = FALSE, clusterDesc = NULL) {

  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)

  opts <- simOptions(x)
  if(opts$antaresVersion < 500) stop("This function only works for study created with Antares 5.0 and newer versions")

  x <- .checkColumns(x, list(areas = "MRG. PRICE",
                             clusters = c("production", "NODU", "NP Cost")))

  if (surplusLastUnit) x <- .checkColumns(x, list(clusters = "availableUnits"))

  # Get marginal, fixed and startup cost of the clusters
  if (is.null(clusterDesc)) clusterDesc <- readClusterDesc(opts)
  .fillClusterDesc(clusterDesc, marginal.cost = 0, fixed.cost = 0, startup.cost = 0)

  idVars <- .idCols(x$clusters)

  tmp <- merge(x$clusters,
               x$areas[, c(setdiff(idVars, "cluster"), "MRG. PRICE"), with = FALSE],
               by = setdiff(idVars, "cluster"))
  tmp <- merge(tmp, clusterDesc, by = c("area", "cluster"))

  # Computed variable, fixed and startup costs
  tmp[, `:=`(
    variableCost = production * marginal.cost,
    fixedCost = NODU * fixed.cost,
    opCost = production * marginal.cost + NODU * fixed.cost
  )]

  setorderv(tmp, .idCols(tmp))
  tmp[, startupCost := pmax(0, NODU - shift(NODU, fill = 0)) * startup.cost]
  tmp[timeId == min(timeId), startupCost := NODU * startup.cost]

  tmp[, `:=`(surplusPerUnit = (`MRG. PRICE` * production - opCost - startupCost) / unitcount,
             totalSurplus = `MRG. PRICE` * production - opCost - startupCost,
             nbHoursPMax = production == round(nominalcapacity * unitcount))]

  tmp[, economicGradient := surplusPerUnit / nominalcapacity]

  if (surplusLastUnit) {
    tmp[, prodLastUnit := pmax(0, (NODU == availableUnits) * (production - nominalcapacity * (NODU - 1)))]
    tmp[, surplusLastUnit := (prodLastUnit > 0) * (`MRG. PRICE` * prodLastUnit - opCost / pmax(1, NODU) - startup.cost * (startupCost > 0))]
    res <- tmp[, c(idVars, "surplusPerUnit", "surplusLastUnit", "totalSurplus",
                   "economicGradient"),
               with = FALSE]
  } else {
    res <- tmp[, c(idVars, "variableCost", "fixedCost", "startupCost", "surplusPerUnit",
                   "totalSurplus", "economicGradient"),
               with = FALSE]
  }

  # Set correct attributes to the result
  res <- .addClassAndAttributes(res, FALSE, "hourly", opts, type = "surplusClusters")

  res <- changeTimeStep(res, timeStep)
  if (synthesis) res <- .aggregateMcYears(res)

  res
}
