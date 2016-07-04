#' Upward and downward margins for an area
#'
#' This function computes the upward and downward margins for each area at a
#' given time step. Upward margin represents ... Downward margin represents ...
#' There are two versions of both indicators : isolated and interconnected.
#' The only difference is that the interconnected version takes into account
#' the balance of the area.
#'
#' @param x
#'   An object created with function \code{\link[antaresRead]{readAntares}}. It
#'   must contain data for areas and for clusters. More specifically this
#'   function requires the columns \code{hstorPMaxAvg},
#'   \code{thermalAvailability} and \code{mustRunPartial}. To get these columns,
#'   one has to invoke \code{\link[antaresRead]{readAntares}} with parameters:
#'   \code{mustRun = TRUE, thermalAvailabilities = TRUE, hydroStorageMaxPower = TRUE}.
#'   (see examples).
#' @param ignoreMustRun
#'   Should must run productions be ignored in the computation? It should be
#'   \code{TRUE} only if the studied areas have no clusters in must run.
#' @inheritParams surplusClusters
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{thermalAvailability}{Sum of thermal availabilities of all cluster of an area.\cr
#'                      formula = sum(thermalAvailability) }
#' \item{thermalPmin}{
#'   Sum of thermal minimum power of all clusters of an area. The minimum power
#'   of a cluster is defined as the maximum of the partial must run and the
#'   minimum stable power of the cluster. If \code{ignoreMustRun = TRUE}, it
#'   is simply equal to the minimum stable power of the cluster.\cr
#'                      formula = pmax(min.stable.power*unitcount, mustRunPartial)
#' }
#' \item{pumping}{}
#' \item{storage}{}
#' \item{isolatedUpwardMargin}{\cr
#'                      formula = thermalAvailability + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD}
#' \item{isolatedDownwardMargin}{\cr
#'                      formula = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD}
#' \item{interconnectedUpwardMargin}{\cr
#'                      formula = isolatedUpwardMargin - BALANCE}
#' \item{interconnectedDownwardMargin}{\cr
#'                      formula = isolatedDownwardMargin + BALANCE}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", clusters = "all", mustRun = TRUE,
#'                       thermalAvailabilities = TRUE,
#'                       hydroStorageMaxPower = TRUE)
#' margins(mydata)
#'
#' # If there is no must run in the study, use ignoreMustRun to avoid useless
#' # data importation and computations
#'
#' mydata <- readAntares(areas = "all", clusters = "all",
#'                       thermalAvailabilities = TRUE,
#'                       hydroStorageMaxPower = TRUE)
#' margins(mydata, ignoreMustRun = TRUE)
#'
#' # Example that minimises the data imported
#' mydata <- readAntares(areas = "all", clusters = "all",
#'                       thermalAvailabilities = TRUE,
#'                       hydroStorageMaxPower = TRUE,
#'                       select = c("H. ROR", "WIND", "SOLAR", "MISC. DTG",
#'                                  "LOAD", "BALANCE"))
#'
#' margins(mydata, ignoreMustRun = TRUE)
#' }
#'
#' @export
#'
margins <- function(x, ignoreMustRun = FALSE, clusterDesc = NULL) {
  if (!is(x, "antaresDataList")) stop ("'x' is not an object of class 'antaresDataList'")

  # Check that x contains the needed variables
  neededCol <- list(clusters = c("thermalAvailability"))
  if (!ignoreMustRun) neededCol$clusters <- append(neededCol$clusters, "mustRunPartial")

  neededColArea <- c("hstorPMaxAvg", "H. ROR", "WIND", "SOLAR", "MISC. DTG", "LOAD", "BALANCE")

  if(is.null(x$areas) & is.null(x$districts)) stop("'x' has to contain 'area' and/or 'district' data")
  if (!is.null(x$areas)) neededCol$areas <- neededColArea
  if (!is.null(x$districts)) neededCol$districts <- neededColArea

  x <- .checkColumns(x, neededCol)

  opts <- simOptions(x)

  if (!is.null(x$areas)) idVars <- .idCols(x$areas)
  else {
    idVars <- .idCols(x$districts)
    idVars[idVars == "district"] <- "area"
  }

  if (ignoreMustRun) {
    clusters <- x$clusters[, c(idVars, "cluster", "thermalAvailability"), with = FALSE]
    clusters$mustRunPartial <- 0
  } else {
    clusters <- x$clusters[, c(idVars, "cluster", "thermalAvailability",
                               "mustRunPartial"), with = FALSE]
  }

  # Construt the required intermediary tables
  #
  # Cluster available power
  available <- clusters[, .(thermalAvailability = sum(thermalAvailability)),
                        by = idVars]

  #Step disponibility
  #
  # Step disponibility is equal to the transmission capacity of a
  #link between a real area and a storage area. One has to be carefull with the
  # direction of the link.
  # If there is no pumped storage virtual areas, then step disponibility is equal
  # to 0.
  if(!is.null(attr(x, "virtualNodes")) &&
     !is.null(attr(x, "virtualNodes")$storageFlexibility)) {

    x <- .checkColumns(x, list(links = c("transCapacityDirect", "transCapacityIndirect")))
    vareas <- attr(x, "virtualNodes")$storageFlexibility
    pspLinks <- x$links[link %in% getLinks(vareas)]

    linksFromTo <- tstrsplit(pspLinks$link, split = " - ")
    pspLinks$area <- linksFromTo[[1]]
    pspLinks$to <- linksFromTo[[2]]

    # If the link connects a virtual node to a real node, we reverse it, so that
    # all links have the same direction: real node to virtual node.
    pspLinks[area %in% vareas,
             `:=`(
               area = to,
               to = area,
               transCapacityDirect = transCapacityIndirect,
               transCapacityIndirect = transCapacityDirect
             )]

    # Users tend to use a transmission capacity of 1 instead of 0 to avoid warnings.
    # The following lines correct this.
    pspLinks[transCapacityIndirect == 1, transCapacityIndirect := 0]
    pspLinks[transCapacityDirect == 1, transCapacityDirect := 0]

    # Aggregate transmissions capacities
    stepCapacity <- pspLinks[, .(pumping = sum(transCapacityDirect),
                                 storage = sum(transCapacityIndirect)),
                             by = idVars]

    stepCapacity[is.na(pumping), c("pumping", "storage") := 0]

  } else {
    stepCapacity <- NULL
  }

  # Thermal minimal power
  #
  # For a given cluster, Pmin is the maximum of Min Stable Power and partial
  # must run.
  if (is.null(clusterDesc)) clusterDesc <- readClusterDesc(opts)
  if (is.null(clusterDesc$min.stable.power)) clusterDesc[, min.stable.power := 0]
  clusterDesc[is.na(min.stable.power), min.stable.power := 0]

  clusters <- merge(clusters, clusterDesc[, .(area, cluster, min.stable.power,unitcount)],
                    by = c("area", "cluster"))
  clusters[, thermalPmin := pmax(min.stable.power*unitcount, mustRunPartial)]

  thermalPmin <- clusters[, .(thermalPmin = sum(thermalPmin)), by = idVars]

  # Put all together !
  intermediaryData <- merge(available, thermalPmin, by = idVars, all = TRUE)
  if (!is.null(stepCapacity)) {
    intermediaryData <- merge(intermediaryData, stepCapacity, by = idVars, all = TRUE)
  } else {
    intermediaryData[, c("pumping", "storage") := 0]
  }



  # Effective computation of the margins

  res <- list()

  if (!is.null(x$areas)) {
    # Create the main table that will be used to compute the margins
    data <- x$areas[, c(
      idVars,
      c("hstorPMaxAvg", "H. ROR", "WIND","SOLAR", "MISC. DTG",
        "LOAD", "BALANCE")
    ),
    with = FALSE]

    # Add intermediary data
    data <- merge(data, intermediaryData, by = idVars, all.x = TRUE)
    data[is.na(thermalAvailability), thermalAvailability := 0]
    data[is.na(pumping), c("pumping", "storage") := 0]
    data[is.na(thermalPmin), thermalPmin := 0]

    # Compute margins
    data[,`:=`(
      isolatedUpwardMargin = thermalAvailability + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD,
      isolatedDownwardMargin = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD
    )]

    data[, `:=`(
      interconnectedUpwardMargin = isolatedUpwardMargin - BALANCE,
      interconnectedDownwardMargin = isolatedDownwardMargin + BALANCE
    )]

    # Add the results to the final object returned by the function
    res$areas <- data[, c(idVars, "thermalAvailability", "thermalPmin", "pumping", "storage",
                    "isolatedUpwardMargin", "isolatedDownwardMargin",
                    "interconnectedUpwardMargin", "interconnectedDownwardMargin"),
                with = FALSE]

    res$areas <- .setAttrs(res$areas, "margins", opts, timeStep = attr(x, "timeStep"),
                           synthesis = attr(x, "synthesis"))
  }

  if (!is.null(x$districts)) {
    idVars[idVars == "area"] <- "district"

    # Create the main table that will be used to compute the margins
    data <- x$districts[, c(
      idVars,
      c("hstorPMaxAvg", "H. ROR", "WIND","SOLAR", "MISC. DTG",
        "LOAD", "BALANCE")
    ),
    with = FALSE]

    # Add intermediary data
    data <- merge(data, .groupByDistrict(intermediaryData, opts), by = idVars, all.x = TRUE)
    data[is.na(thermalAvailability), thermalAvailability := 0]
    data[is.na(pumping), c("pumping", "storage") := 0]
    data[is.na(thermalPmin), thermalPmin := 0]

    # Compute margins
    data[,`:=`(
      isolatedUpwardMargin = thermalAvailability + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD,
      isolatedDownwardMargin = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD
    )]

    data[, `:=`(
      interconnectedUpwardMargin = isolatedUpwardMargin - BALANCE,
      interconnectedDownwardMargin = isolatedDownwardMargin + BALANCE
    )]

    # Add the results to the final object returned by the function
    res$districts <- data[, c(idVars, "thermalAvailability", "thermalPmin", "pumping", "storage",
                             "isolatedUpwardMargin", "isolatedDownwardMargin",
                             "interconnectedUpwardMargin", "interconnectedDownwardMargin"),
                      with = FALSE]

    res$districts <- .setAttrs(res$districts, "margins", opts, timeStep = attr(x, "timeStep"),
                               synthesis = attr(x, "synthesis"))
  }

  if (length(res) == 1) return(res[[1]])

  class(res) <- append(c("antaresDataList", "antaresData"), class(res))
  attr(res, "timeStep") <- attr(x, "timeStep")
  attr(res, "synthesis") <- attr(x, "synthesis")
  attr(res, "opts") <- simOptions(x)

  res
}
