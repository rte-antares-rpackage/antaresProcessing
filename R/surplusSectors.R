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
#'   vector containing the name of the sector wor which surplus needs to be
#'   computed. Possible values are "thermal", "WIND", "SOLAR", "H. ROR", "H. STOR".
#'   If it contains the value "thermal", then the parameter \code{x} has to contain
#'   cluster data.
#' @inheritParams surplus
#'
#' @return
#' A data.table of class "antaresData". Ir contains one column per sector
#' containing the surplus of that sector for a given area and timeId.
#'
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", clusters = "all")
#' surplusSectors(mydata)
#' }
#'
#' @export
#'
surplusSectors <- function(x, sectors = c("thermal", "WIND", "SOLAR", "H. ROR", "H. STOR"),
                           timeStep = "annual") {

  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)

  opts <- simOptions(x)

  fatalProdVars <- intersect(sectors, c("WIND", "SOLAR", "H. ROR", "H. STOR"))
  idVars <- intersect(names(x$areas), antares:::pkgEnv$idVars)

  if (length(fatalProdVars) > 0) {
    x <- .checkColumns(x, list(areas = c("MRG. PRICE", fatalProdVars)))

    res <- x$areas[, c(idVars, fatalProdVars, "MRG. PRICE"), with = FALSE]

    for (v in fatalProdVars) res[, c(v) := get(v) * `MRG. PRICE`]

    res$`MRG. PRICE` <- NULL
  } else {
    res <- NULL
  }

  if("thermal" %in% sectors) {
    surplusThermal <- surplusClusters(x, "hourly")

    # Add column group
    clusterDesc <- readClusterDesc()
    surplusThermal <- merge(surplusThermal,
                            clusterDesc[, .(area, cluster, group)],
                            by = c("area", "cluster"))

    # Aggregate by group
    surplusThermal <- surplusThermal[,.(surplus = sum(totalSurplus)),
                                     by = c(idVars, "group")]

    formula <- sprintf("%s ~ group", paste(idVars, collapse= " + "))
    surplusThermal <- dcast(surplusThermal, as.formula(formula),
                            value.var = "surplus", fill = 0)

    if (is.null(res)) res <- surplusThermal
    else res <- merge(res, surplusThermal, by = idVars)
  }

  # Add to the name of the columns the prefix "surplus"
  cols <- setdiff(names(res), idVars)
  setnames(res, cols, paste0("surplus", cols))

  # Set correct attributes to the result
  class(res) <- c("antaresDataTable", "antaresData", "data.table", "data.frame")
  attr(res, "timeStep") <- "hourly"
  attr(res, "synthesis") <- FALSE
  attr(res, "opts") <- opts
  attr(res, "type") <- "surplusSectors"

  changeTimeStep(res, timeStep)

}
