#' Upward and downward margins for an area
#'
#' This function computes the upward and downward margins for each area at a
#' given time step. Upward margin represents ... Downward margin represents ...
#' There are two versions of both indicators : isolated and interconnected.
#' The only difference is that the interconnected version takes into account
#' the balance of the area.
#'
#' @param x
#'   an object created with function \code{\link[antaresRead]{readAntares}}. It
#'   must contain data for areas and for clusters. More specifically this
#'   function requires the columns \code{hstorPMaxAvg},
#'   \code{thermalAvailability} and \code{mustRunPartial}. To get these columns,
#'   one has to invoke \code{\link[antaresRead]{readAntares}} with parameters:
#'   \code{mustRun = TRUE, thermalAvailabilities = TRUE, hydroStorageMaxPower = TRUE}.
#'   (see examples).
#' @param ignoreMustRun
#'   Should must run productions be ignored in the computation ? It should be
#'   \code{TRUE} only if the studied areas have no clusters in must run.
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{thermalAvailability}{sum of thermal availabilities of all cluster of an area.}
#' \item{thermalPmin}{
#'   Sum of thermal minimum power of all clusters of an area. The minimum power
#'   of a cluster is defined as the maximum of the partial must run and the
#'   minimum stable power of the cluster. If \code{ignoreMustRun = TRUE}, it
#'   is simply equal to the minimum stable power of the cluster.
#' }
#' \item{pumping}{}
#' \item{storage}{}
#' \item{isolatedUpwardMargin}{}
#' \item{isolatedDownwardMargin}{}
#' \item{interconnectedUpwardMargin}{}
#' \item{interconnectedDownwardMargin}{}
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
#' }
#'
#' @export
#'
margins <- function(x, ignoreMustRun = FALSE) {
  x <- .checkColumns(x, list(areas = c("hstorPMaxAvg", "H. ROR", "WIND",
                                       "SOLAR", "MISC. DTG", "LOAD", "BALANCE"),
                             clusters = c("thermalAvailability")))

  opts <- simOptions(x)
  idVars <- .idCols(x$areas)

  # Create the main table that will be used to compute the margins
  data <- x$areas[, c(
                      idVars,
                      c("hstorPMaxAvg", "H. ROR", "WIND","SOLAR", "MISC. DTG",
                        "LOAD", "BALANCE")
                    ),
                  with = FALSE]

  if (ignoreMustRun) {
    clusters <- x$clusters[, c(idVars, "cluster", "thermalAvailability"), with = FALSE]
    clusters$mustRunPartial <- 0
  } else {
    clusters <- x$clusters[, c(idVars, "cluster", "thermalAvailability",
                               "mustRunPartial"), with = FALSE]
  }

  # Add cluster available power.
  available <- clusters[, .(thermalAvailability = sum(thermalAvailability)),
                        by = idVars]
  data <- merge(data, available, by = idVars, all.x = TRUE)
  data[is.na(thermalAvailability), thermalAvailability := 0]

  # Compute step disponibility. It is equal to the transmission capacity of a
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

    # add transmission capacities
    data <- merge(data, stepCapacity, by = idVars)

  } else {
    # If there is no pumped storage node, then pumping and storage are equal to 0
    data[, c("pumping", "storage") := 0]
  }

  # Compute thermalPmin
  # For a given cluster, Pmin is the maximum of Min Stable Power and partial
  # must run.
  clusterDesc <- readClusterDesc(opts)
  if (is.null(clusterDesc$min.stable.power)) clusterDesc[, min.stable.power := 0]
  clusterDesc[is.na(min.stable.power), min.stable.power := 0]

  clusters <- merge(clusters, clusterDesc[, .(area, cluster, min.stable.power)],
                    by = c("area", "cluster"))
  clusters[, thermalPmin := pmax(min.stable.power, mustRunPartial)]

  thermalPmin <- clusters[, .(thermalPmin = sum(thermalPmin)), by = idVars]
  data <- merge(data, thermalPmin, by = idVars)

  # Finally compute margins !
  data[,`:=`(
    isolatedUpwardMargin = thermalAvailability + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD,
    isolatedDownwardMargin = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. DTG` - LOAD
  )]

  data[, `:=`(
    interconnectedUpwardMargin = isolatedUpwardMargin - BALANCE,
    interconnectedDownwardMargin = isolatedDownwardMargin + BALANCE
  )]

  data[, c(idVars, "thermalAvailability", "thermalPmin", "pumping", "storage",
           "isolatedUpwardMargin", "isolatedDownwardMargin",
           "interconnectedUpwardMargin", "interconnectedDownwardMargin"),
       with = FALSE]
}
