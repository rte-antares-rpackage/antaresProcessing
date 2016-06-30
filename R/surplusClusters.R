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
#' \item{surplusPerUnit}{Average surplus per unit of the cluster.\cr
#'  formula = (`MRG. PRICE` * production - prodCost - startupCost) / unitcount }
#' \item{surplusLastUnit}{Surplus of the last unit of the cluster.\cr
#' formula = (`MRG. PRICE` * prodLastUnit - prodCost / pmax(1, NODU) - startup.cost)}
#' \item{totalSurplus}{Surplus of all units of the cluster.\cr
#' formula = `MRG. PRICE` * production - prodCost - startupCost}
#' \item{nbHoursGeneration}{It represents the production of a cluster expressed
#' in number of hours of production at the total capacity of the cluster. It is
#' equal to the production divided by the capacity of the cluster.\cr
#' formula = production / (unitcount * nominalcapacity)}
#' \item{economicGradient}{Economic gradient of a cluster. It is equal to
#' the surplus per unit divided by the capacity of a unit.\cr
#' formula = surplusPerUnit / nominalcapacity }
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
  clusterDesc[is.na(marginal.cost), marginal.cost := 0]

  if(is.null(clusterDesc$fixed.cost)) clusterDesc$fixed.cost <- 0
  clusterDesc[is.na(fixed.cost), fixed.cost := 0]

  if(is.null(clusterDesc$startup.cost)) clusterDesc$startup.cost <- 0
  clusterDesc[is.na(startup.cost), startup.cost := 0]

  idVars <- .idCols(x$clusters)

  tmp <- merge(x$clusters,
               x$areas[, c(setdiff(idVars, "cluster"), "MRG. PRICE"), with = FALSE],
               by = setdiff(idVars, "cluster"))
  tmp <- merge(tmp, clusterDesc, by = c("area", "cluster"))

  # Computed variable, fixed and startup costs
  tmp[, prodCost := production * marginal.cost + NODU * fixed.cost]
  tmp[, startupCost := pmax(0, NODU - shift(NODU, fill = 0)) * startup.cost]
  tmp[timeId == min(timeId), startupCost := NODU * startup.cost]

  tmp[, `:=`(surplusPerUnit = (`MRG. PRICE` * production - prodCost - startupCost) / unitcount,
             totalSurplus = `MRG. PRICE` * production - prodCost - startupCost,
             nbHoursGeneration = production / (unitcount * nominalcapacity))]

  tmp[, economicGradient := surplusPerUnit / nominalcapacity]

  if (surplusLastUnit) {
    tmp[, prodLastUnit := pmax(0, (NODU == availableUnits) * (production - nominalcapacity * (NODU - 1)))]
    tmp[, surplusLastUnit := (prodLastUnit > 0) * (`MRG. PRICE` * prodLastUnit - prodCost / pmax(1, NODU) - startup.cost * (startupCost > 0))]
    res <- tmp[, c(idVars, "surplusPerUnit", "surplusLastUnit", "totalSurplus",
                   "nbHoursGeneration", "economicGradient"),
               with = FALSE]
  } else {
    res <- tmp[, c(idVars, "surplusPerUnit", "totalSurplus","nbHoursGeneration",
                   "economicGradient"),
               with = FALSE]
  }

  # Set correct attributes to the result
  res <- .setAttrs(res, "surplusClusters", opts)

  res <- changeTimeStep(res, timeStep)
  if (synthesis) res <- .aggregateMcYears(res)

  res
}
