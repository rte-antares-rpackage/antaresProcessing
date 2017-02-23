#Copyright © 2016 RTE Réseau de transport d’électricité

.neededColArea <- c("hstorPMaxAvg", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "AVL DTG")

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
#'   and \code{mustRunPartial}. To get these columns,
#'   one has to invoke \code{\link[antaresRead]{readAntares}} with parameters:
#'   \code{mustRun = TRUE, hydroStorageMaxPower = TRUE}.
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
#'                      formula = x$areas[, .(`AVL DTG`)]
#' }
#' \item{thermalPmin}{
#'   Sum of thermal minimum power of all clusters of an area. The minimum power
#'   of a cluster is defined as the maximum of the partial must run and the
#'   minimum stable power of the cluster. If \code{ignoreMustRun = TRUE}, it
#'   is simply equal to the minimum stable power of the cluster.\cr
#'                      formula = pmax(min.stable.power*NODU, mustRunTotal)
#' }
#' \item{pumping}{
#' }
#' \item{storage}{
#' }
#' \item{isolatedUpwardMargin}{\cr
#'   formula = `AVL DTG` + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
#' }
#' \item{isolatedDownwardMargin}{\cr
#'   formula = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
#' }
#' \item{interconnectedUpwardMargin}{\cr
#'   formula = isolatedUpwardMargin - BALANCE
#' }
#' \item{interconnectedDownwardMargin}{\cr
#'   formula = isolatedDownwardMargin + BALANCE
#' }
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", clusters = "all", mustRun = TRUE,
#'                       hydroStorageMaxPower = TRUE)
#' margins(mydata)
#'
#' # If there is no must run in the study, use ignoreMustRun to avoid useless
#' # data importation and computations
#'
#' mydata <- readAntares(areas = "all", clusters = "all",
#'                       hydroStorageMaxPower = TRUE)
#' margins(mydata, ignoreMustRun = TRUE)
#'
#' # Example that minimises the data imported
#' mydata <- readAntares(areas = "all", clusters = "all",
#'                       hydroStorageMaxPower = TRUE,
#'                       select = c("H. ROR", "WIND", "SOLAR", "MISC. NDG",
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
  if(is.null(x$areas) & is.null(x$districts)) stop("'x' has to contain 'area' and/or 'district' data")

  neededCol<-list()
  if (!is.null(x$areas)) {
    neededCol$areas <- .neededColArea
  }

  if (!is.null(x$districts)) neededCol$districts <- .neededColArea
  if (!ignoreMustRun) {
    neededCol$clusters <- c("mustRunTotal")
  }

  x <- .checkColumns(x, neededCol)

  opts <- simOptions(x)

  if (!is.null(x$areas)) idVars <- .idCols(x$areas)
  else {
    idVars <- .idCols(x$districts)
    idVars[idVars == "district"] <- "area"
  }

  if (ignoreMustRun) {
    clusters <- x$clusters[, c(idVars, "cluster", "NODU"), with = FALSE]
    clusters$mustRunTotal <- 0
  } else {
    clusters <- x$clusters[, c(idVars, "cluster", "NODU",
                               "mustRunTotal"), with = FALSE]
  }

  #Step disponibility
  #
  # Step disponibility is equal to the transmission capacity of a
  #link between a real area and a storage area. One has to be carefull with the
  # direction of the link.
  # If there is no pumped storage virtual areas, then step disponibility is equal
  # to 0.
  if(!is.null(attr(x, "virtualNodes")) &&
     !is.null(attr(x, "virtualNodes")$storageFlexibility)) {

    x <- .checkColumns(x, list(areas = c("storageCapacity", "pumpingCapacity")))

    stepCapacity <- x$areas[, c(idVars, "storageCapacity", "pumpingCapacity"),
                            with = FALSE]

  } else {
    stepCapacity <- NULL
  }

  # Thermal minimal power
  #
  # For a given cluster, Pmin is the maximum of Min Stable Power and partial
  # must run.
  if (is.null(clusterDesc)) clusterDesc <- readClusterDesc(opts)
  .fillClusterDesc(clusterDesc, min.stable.power = 0)

  intermediaryData <- clusters[clusterDesc[, .(area, cluster, min.stable.power)], thermalPmin := pmax(min.stable.power*NODU, mustRunTotal),  on = c("area", "cluster")][, .(thermalPmin = sum(thermalPmin)), by = idVars]

  if (!is.null(stepCapacity)) {
    intermediaryData <- merge(intermediaryData, stepCapacity, by = idVars, all = TRUE)
  } else {
    intermediaryData[, c("pumpingCapacity", "storageCapacity") := 0]
  }

  # Effective computation of the margins. The function .computeMargins is
  # defined below.
  res <- list()

  if (!is.null(x$areas)) {
    res$areas <- .computeMargins(x$areas, intermediaryData)
    attr(res$areas, "type") <- "areaMargins"
  }

  if (!is.null(x$districts)) {
    res$districts <- .computeMargins(x$districts, .groupByDistrict(intermediaryData, opts))
    attr(res$districts, "type") <- "districtMargins"
  }

  .addClassAndAttributes(res, attr(x, "synthesis"), attr(x, "timeStep"), opts, simplify = TRUE)
}



#' Private function used in function "margins".
#'
#' @param mainDT
#'   an antaresDataTable obkect containing areas or districts.
#' @param additionalDT
#'   data.table with the same id columns and the same number of rows. It must
#'   contain columns thermalPmin, pumpingCapacity, storage and thermalAvailability.
#'
#' @noRd
.computeMargins <- function(mainDT, additionalDT) {
  idVars <- .idCols(mainDT)

  # Create the main table that will be used to compute the margins
  data <- mainDT[, c(idVars, .neededColArea), with = FALSE]

  # Add intermediary data
  data <- merge(data, additionalDT, by = idVars, all.x = TRUE)

  data[is.na(pumpingCapacity), c("pumpingCapacity", "storageCapacity") := 0]

  data[is.na(thermalPmin), thermalPmin := 0]

  # Compute margins
  data[,`:=`(
    isolatedUpwardMargin = `AVL DTG` + hstorPMaxAvg + storageCapacity + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD,
    isolatedDownwardMargin = thermalPmin - pumpingCapacity + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
  )]

  data[, `:=`(
    interconnectedUpwardMargin = isolatedUpwardMargin - BALANCE,
    interconnectedDownwardMargin = isolatedDownwardMargin + BALANCE
  )]


  data[, c(idVars, "AVL DTG", "thermalPmin", "pumpingCapacity", "storageCapacity",
           "isolatedUpwardMargin", "isolatedDownwardMargin",
           "interconnectedUpwardMargin", "interconnectedDownwardMargin"),
       with = FALSE]
}
