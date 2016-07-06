#' Compute the surplus of sectors
#'
#' This function computes the surplus of sectors for each area and time step.
#' For sectors wind, solar, hydraulic storage and run of river, production costs
#' are assumed to be equal to 0.
#'
#' @param x
#'   Object of class \code{antaresData} created with \code{readAntares}. It needs
#'   to contain hourly detailed results of a simulation. Moreover, it must contain
#'   area data and if thermal sectors are required, cluster data.
#' @param sectors
#'   vector containing the name of the sectors for which surplus needs to be
#'   computed. Possible values are "thermal" for thermal sectors(nuclear, coal,..),
#'   "ren" for renewable energie and any column name that can be considered as
#'   a production (for instance production of virtual areas). It is assumed that
#'   the cost of these productions is equal to 0 as for renewable energies.
#'   If the parameter contains the value "thermal", then the parameter
#'   \code{x} has to contain cluster data.
#' @inheritParams surplus
#' @inheritParams surplusClusters
#'
#' @return
#' A data.table of class "antaresData". It contains one column per sector
#' containing the surplus of that sector for a given area and timeId.
#'
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", clusters = "all", synthesis = FALSE)
#' surplusSectors(mydata)
#'
#' # Example that minimizes the quantity of data read
#' mydata <- readAntares(areas = "all", clusters = "all", synthesis = FALSE,
#'                       select = c("WIND", "SOLAR", "H. ROR", "H. STOR", "MRG. PRICE"))
#' surplusSectors(mydata)
#'
#' # Note that if the parameter "sectors" is modified, the function can require
#' # more or less data. For instance, if one only wants surplus for thermal
#' # sectors:
#' mydata <- readAntares(areas = "all", clusters = "all", synthesis = FALSE,
#'                       select = "MRG. PRICE")
#' surplusSectors(mydata, sectors = "thermal")
#'
#' }
#'
#' @export
#'
surplusSectors <- function(x, sectors = c("thermal", "renewable"),
                           timeStep = "annual", synthesis = FALSE,
                           groupByDistrict = FALSE, clusterDesc = NULL) {

  .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)
  x <- .checkColumns(x, list(areas = "MRG. PRICE"))

  opts <- simOptions(x)

  if (any(sectors == "renewable")) {
    sectors <- sectors[!sectors == "renewable"]
    sectors <- union(sectors, pkgEnv$ren)
  }

  fatalProdVars <- intersect(sectors, names(x$areas))
  idVars <- .idCols(x$areas)

  if (length(fatalProdVars) > 0) {
    x <- .checkColumns(x, list(areas = c(fatalProdVars)))

    res <- x$areas[, c(idVars, fatalProdVars, "MRG. PRICE"), with = FALSE]

    for (v in fatalProdVars) res[, c(v) := get(v) * `MRG. PRICE`]

    res$`MRG. PRICE` <- NULL

    # Put the result in long format
    res <- melt(res, id.vars = idVars, variable.name = "sector", value.name = "surplus")
    res$cost <- 0
  } else {
    res <- NULL
  }

  if("thermal" %in% sectors) {
    surplusThermal <- surplusClusters(x, "hourly")

    # Add column group
    if (is.null(clusterDesc)) clusterDesc <- readClusterDesc(opts)
    surplusThermal <- merge(surplusThermal,
                            clusterDesc[, .(area, cluster, sector = group)],
                            by = c("area", "cluster"))

    # Aggregate by group
    surplusThermal <- surplusThermal[,.(surplus = sum(totalSurplus),
                                        cost = sum(variableCost + fixedCost + startupCost)),
                                     by = c(idVars, "sector")]

    if (is.null(res)) res <- surplusThermal
    else res <- rbind(res, surplusThermal)
  }

  # Group by district
  if (groupByDistrict) res <- .groupByDistrict(res, opts)

  # Set correct attributes to the result
  res <- .addClassAndAttributes(res, FALSE, "hourly", opts, type = "surplusSectors")

  res <- changeTimeStep(res, timeStep)

  if (synthesis) res <- .aggregateMcYears(res)

  res
}
