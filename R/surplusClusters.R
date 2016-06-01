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
#' @param timeStep
#'   Desired time step for the result
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{cluster}{Cluster name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{surplus}{Surplus of a given cluster at a given time step.}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", clusters = "all", select = "MRG. PRICE")
#'
#' surplusClusters(mydata)
#'
#' }
#'
#' @export
#'
surplusClusters <- function(x, timeStep="annual") {

  opts <- simOptions(x)
  if(opts$antaresVersion < 500) stop("This function only works for study created with Antares 5.0 and newer versions")

  x <- .checkColumns(x, list(areas = "MRG. PRICE",
                             clusters = c("production", "NODU", "NP Cost")))


  if (attr(x, "timeStep") != "hourly") stop("'x' needs to have a hourly time step")

  # Get marginal, fixed and startup cost of the clusters
  clusterDesc <- readClusterDesc(opts)
  clusterDesc[is.na(marginal.cost), marginal.cost := 0]

  if(is.null(clusterDesc$fixed.cost)) clusterDesc$fixed.cost <- 0
  clusterDesc[is.na(fixed.cost), fixed.cost := 0]

  if(is.null(clusterDesc$startup.cost)) clusterDesc$startup.cost <- 0
  clusterDesc[is.na(startup.cost), startup.cost := 0]


  idCols <- intersect(names(x$clusters), antares:::pkgEnv$idVars)

  tmp <- merge(x$clusters,
               x$areas[, mget(c(setdiff(idCols, "cluster"), "MRG. PRICE"))],
               by = setdiff(idCols, "cluster"))
  tmp <- merge(tmp, clusterDesc, by = c("area", "cluster"))

  # Computed variable, fixed and startup costs
  tmp[, prodCost := production * marginal.cost + NODU * fixed.cost]
  tmp[, startupCost := max(0, NODU - shift(NODU, fill = 0)) * startup.cost]



  res <- tmp[, append(mget(idCols),
                      .(surplusPerUnit = (`MRG. PRICE` * production - prodCost - startupCost) / unitcount,
                        surplusLastUnit = ifelse(NODU == unitcount, (`MRG. PRICE` * (production - (NODU - 1) * nominalcapacity) - prodCost / NODU - startupCost * (startupCost > 0)), 0),
                        totalSurplus=`MRG. PRICE` * production - prodCost - startupCost))]

  # Set correct attributes to the result
  class(res) <- c("antaresDataTable", "antaresData", "data.table", "data.frame")
  attr(res, "timeStep") <- "hourly"
  attr(res, "synthesis") <- attr(res, "synthesis")
  attr(res, "opts") <- opts
  attr(res, "type") <- "surplus"

  changeTimeStep(res, timeStep)

}
