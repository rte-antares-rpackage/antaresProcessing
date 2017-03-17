#Copyright © 2016 RTE Réseau de transport d’électricité

.neededColAreaBoth<-c("hstorPMaxAvg", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "AVL DTG")
.neededColAreaUp<-c("hstorPMaxAvg", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "AVL DTG")
.neededColAreaDown<-c("H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE")
.neededColSTEP<-c("storageCapacity", "pumpingCapacity")
.typeBoth<-"both"
.typeUpward<-"upward"
.typeDownward<-"downward"

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
#'
#' @param type
#'   Type of margins to compute. Possibilities :
#'
#'   \itemize{
#'     \item{both}{upward and downward margins are computed}
#'     \item{upward}{only upward margins}
#'     \item{downward}{only downward margins}
#'   }
#'
#' @inheritParams surplusClusters
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{thermalAvailability}{Sum of thermal availabilities of all cluster of an area.
#'
#'                      formula = x$areas[, .(`AVL DTG`)]
#' }
#' \item{thermalPmin}{
#'   Sum of thermal minimum power of all clusters of an area. The minimum power
#'   of a cluster is defined as the maximum of the partial must run and the
#'   minimum stable power of the cluster. If \code{ignoreMustRun = TRUE}, it
#'   is simply equal to the minimum stable power of the cluster.
#'
#'                      formula = pmax(min.stable.power*NODU, mustRunTotal)
#' }
#' \item{pumping}{
#' }
#' \item{storage}{
#' }
#' \item{isolatedUpwardMargin}{
#'   formula = `AVL DTG` + hstorPMaxAvg + storage + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
#' }
#' \item{isolatedDownwardMargin}{
#'   formula = thermalPmin - pumping + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
#' }
#' \item{interconnectedUpwardMargin}{
#'   formula = isolatedUpwardMargin - BALANCE
#' }
#' \item{interconnectedDownwardMargin}{
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
margins <- function(x, type = "upward", ignoreMustRun = FALSE, clusterDesc = NULL) {
  if (!is(x, "antaresDataList")) stop ("'x' is not an object of class 'antaresDataList'")

  # Check that x contains the needed variables
  if(is.null(x$areas) & is.null(x$districts)) stop("'x' has to contain 'area' and/or 'district' data")

  neededCol<-list()
  if (!is.null(x$areas)) {
    if(type==.typeUpward){
      neededCol$areas <- .neededColAreaUp
    }else if(type==.typeDownward){
      neededCol$areas <- .neededColAreaDown
    }else{
      neededCol$areas <- .neededColAreaBoth
    }
  }

  #TODO : a factoriser avec la condition "areas"
  if (!is.null(x$districts)) {
    if(type==.typeUpward){
      neededCol$districts <- .neededColAreaUp
    }else if(type==.typeDownward){
      neededCol$districts <- .neededColAreaDown
    }else{
      neededCol$districts <- .neededColAreaBoth
    }
  }

  if (!ignoreMustRun) {
    neededCol$clusters <- c("mustRunTotal")
  }

  x <- .checkColumns(x, neededCol)

  opts <- simOptions(x)

  if (!is.null(x$areas)) idVars <- getIdCols(x$areas)
  else {
    idVars <- getIdCols(x$districts)
    idVars[idVars == "district"] <- "area"
  }

  if (ignoreMustRun & type %in% c(.typeDownward, .typeBoth)) {
    clusters <- x$clusters[, c(idVars, "cluster", "NODU"), with = FALSE][, mustRunTotal:=0]
  }else if(!ignoreMustRun & type %in% c(.typeDownward, .typeBoth)){
    clusters <- x$clusters[, c(idVars, "cluster", "NODU",
                               "mustRunTotal"), with = FALSE]
  }else{
    clusters<-NULL
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

    if (!is.null(x$areas)){
      x <- .checkColumns(x, list(areas=.neededColSTEP ))
      stepCapacity <- x$areas[, c(idVars, .neededColSTEP),
                                with = FALSE]
    }else{
      stop("when there is virtual areas 'x' has to contain 'area'")
    }
  } else {
    stepCapacity <- NULL
  }

  # Thermal minimal power
  #
  # For a given cluster, Pmin is the maximum of Min Stable Power and partial
  # must run.
  if (is.null(clusterDesc) & type %in% c(.typeDownward, .typeBoth)) clusterDesc <- readClusterDesc(opts)

  if(type %in% c(.typeDownward, .typeBoth)){
    .fillClusterDesc(clusterDesc, min.stable.power = 0)
    intermediaryData <- clusters[clusterDesc[, .(area, cluster, min.stable.power)], thermalPmin := pmax(min.stable.power*NODU, mustRunTotal),  on = c("area", "cluster")][, .(thermalPmin = sum(thermalPmin)), by = idVars]
  }else{
    intermediaryData<-NULL
  }

  if (!is.null(stepCapacity) & !is.null(intermediaryData)){
    intermediaryData <- merge(intermediaryData, stepCapacity, by = idVars, all = TRUE)
  }else if(!is.null(stepCapacity) & is.null(intermediaryData)){
    intermediaryData <- stepCapacity
  }else if(is.null(stepCapacity) & !is.null(intermediaryData)){
    intermediaryData<-intermediaryData
  }else{
      intermediaryData<-NULL
  }

  # Effective computation of the margins. The function .computeMargins is
  # defined below.
  res <- list()

  if (!is.null(x$areas)) {
    res$areas <- .computeMargins(x$areas, intermediaryData, type)
    attr(res$areas, "type") <- "areaMargins"
  }

  if (!is.null(x$districts)) {
    if(!is.null(intermediaryData)){
      intermediaryDataDistrict<-.groupByDistrict(intermediaryData, opts)
    }else{
      intermediaryDataDistrict<-NULL
    }
    res$districts <- .computeMargins(x$districts, intermediaryDataDistrict, type)
    attr(res$districts, "type") <- "districtMargins"
  }

  .addClassAndAttributes(res, attr(x, "synthesis"), attr(x, "timeStep"), opts, simplify = TRUE)
}



#' Private function used in function "margins".
#'
#' @param mainDT
#'   an antaresDataTable object containing areas or districts.
#' @param additionalDT
#'   data.table with the same id columns and the same number of rows. It must
#'   contain columns thermalPmin, pumpingCapacity, storage and thermalAvailability.
#'
#' @noRd
.computeMargins <- function(mainDT, additionalDT, type) {
  idVars <- .idCols(mainDT)

  if(type==.typeUpward){
    neededColMaintDT <- .neededColAreaUp
  }else if(type==.typeDownward){
    neededColMaintDT <- .neededColAreaDown
  }else{
    neededColMaintDT <- .neededColAreaBoth
  }

  # Create the main table that will be used to compute the margins
  myData <- mainDT[, c(idVars, neededColMaintDT), with = FALSE]

  # Add intermediary data
  if(!is.null(additionalDT)){
    myData <- additionalDT[myData, on=idVars]
    if(!is.null(myData$pumpingCapacity)){
      myData[is.na(pumpingCapacity),  eval(.neededColSTEP) := 0]
    }
    if(type %in% c(.typeDownward,.typeBoth)){
      myData[is.na(thermalPmin), thermalPmin := 0]
    }
  }

  # Compute margins without STEP
  if(type %in% c(.typeUpward,.typeBoth) & !is.null(myData$storageCapacity)){
    myData[,`:=`(
      isolatedUpwardMargin = `AVL DTG` + hstorPMaxAvg + storageCapacity + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
    )]
    myData[,interconnectedUpwardMargin := isolatedUpwardMargin - BALANCE]
  }

  if(type %in% c(.typeDownward,.typeBoth) & !is.null(myData$pumpingCapacity)){
    myData[,`:=`(
      isolatedDownwardMargin = thermalPmin - pumpingCapacity + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
    )]
    myData[,interconnectedDownwardMargin := isolatedDownwardMargin + BALANCE]
  }

  # Compute margins with STEP
  if(type %in% c(.typeUpward,.typeBoth)  & is.null(myData$storageCapacity)){
    myData[,`:=`(
      isolatedUpwardMargin = `AVL DTG` + hstorPMaxAvg + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
    )]
    myData[,interconnectedUpwardMargin := isolatedUpwardMargin - BALANCE]
  }

  if(type %in% c(.typeDownward,.typeBoth)  & is.null(myData$pumpingCapacity)){
    myData[,`:=`(
      isolatedDownwardMargin = thermalPmin + `H. ROR` + WIND + SOLAR + `MISC. NDG` - LOAD
    )]
    myData[,interconnectedDownwardMargin := isolatedDownwardMargin + BALANCE]
  }

  if(type==.typeUpward  & !is.null(myData$storageCapacity)){
    myData[, c(idVars, "AVL DTG", "hstorPMaxAvg", "storageCapacity",
             "isolatedUpwardMargin",
             "interconnectedUpwardMargin"),
         with = FALSE]
  }else if(type==.typeUpward  & is.null(myData$storageCapacity)){
    myData[, c(idVars, "AVL DTG", "hstorPMaxAvg",
             "isolatedUpwardMargin",
             "interconnectedUpwardMargin"),
         with = FALSE]
  }else if(type==.typeDownward & !is.null(myData$pumpingCapacity)){
    myData[, c(idVars, "thermalPmin", "pumpingCapacity",
             "isolatedDownwardMargin",
             "interconnectedDownwardMargin"),
         with = FALSE]
  }else if(type==.typeDownward & is.null(myData$pumpingCapacity)){
    myData[, c(idVars, "thermalPmin",
             "isolatedDownwardMargin",
             "interconnectedDownwardMargin"),
         with = FALSE]
  }else if(type==.typeBoth & is.null(myData$storageCapacity) & is.null(myData$pumpingCapacity)){
    myData[, c(idVars, "AVL DTG", "thermalPmin", "hstorPMaxAvg",
             "isolatedUpwardMargin",
             "interconnectedUpwardMargin",
             "isolatedDownwardMargin",
             "interconnectedDownwardMargin"),
         with = FALSE]
  }else{
    myData[, c(idVars, "AVL DTG", "thermalPmin", "hstorPMaxAvg", "pumpingCapacity", "storageCapacity",
             "isolatedUpwardMargin",
             "interconnectedUpwardMargin",
             "isolatedDownwardMargin",
             "interconnectedDownwardMargin"),
         with = FALSE]
  }

}
