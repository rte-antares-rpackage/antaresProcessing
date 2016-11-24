#Copyright © 2016 RTE Réseau de transport d’électricité

.neededColAreaExternalDepandancies <- c("netLoad", "AVL DTG", "hstorPMaxAvg")

#' External Dependancies in imports and exports
#'
#' This function computes the depandance in imports and export for each area or districts at a
#' given time step. Depandance in imports represents moments where imports are required
#' to have no loss of load. Depandance in exports represents moments where exports are required to
#' have no spilled energy.
#'
#' @param x
#'   An object created with function \code{\link[antaresRead]{readAntares}}. It
#'   must contain data for areas and/or distritcs . More specifically this
#'   function requires the columns \code{hstorPMaxAvg},
#'   and \code{netLoad}. To get these columns,
#'   one has to invoke \code{\link[antaresRead]{readAntares}} with the parameter \code{hydroStorageMaxPower = TRUE}
#'   and \code{\link[antaresProcessing]{addNetLoad}}
#'   (see examples).
#'
#'   Moreover it needs to have a hourly time step.
#'
#'   This object must also contain linkCapacity if there was virtual areas remove by \code{\link[antaresRead]{removeVirtualAreas}}
#'   to be able to calculate pumping and storage capacities.
#' @param timeStep
#'   Desired time step for the result.
#' @param synthesis
#'   If TRUE, average external dependncies are returned. Else the function returns external dependncies
#'   per Monte-Carlo scenario.
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{pumping}{capacity of pumping}
#' \item{storage}{capacity of storage}
#' \item{exportsLevel}{netLoad + pumping}
#' \item{importsLevel}{netLoad - `AVL DTG` - hydroStorageMaxPower - storage > 0}
#' \item{exportsFrequency}{number of time step where this criteria is satisfied\cr
#'   criteria : netLoad + pumping < 0
#' }
#' \item{importsFrequency}{number of time step where this criteria is satisfied\cr
#'   criteria : netLoad - `AVL DTG` - hydroStorageMaxPower - storage > 0
#' }
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all",
#'                       hydroStorageMaxPower = TRUE, mustRun = TRUE)
#' addNetLoad(mydata)
#' externalDependancies(mydata)
#'
#' mydata <- readAntares(districts = "all",
#'                       hydroStorageMaxPower = TRUE, mustRun = TRUE)
#' addNetLoad(mydata)
#' externalDependancies(mydata)
#'
#' # if there are some virtual areas, include link and linkCapacity
#' mydata <- readAntares(areas = "all", link = "all", linkCapacity=TRUE,
#'                       hydroStorageMaxPower = TRUE, mustRun = TRUE)
#' addNetLoad(mydata)
#' removeVirtalAreas(mydata)
#' externalDependancies(mydata, ignoreMustRun = TRUE)
#'
#' # Example that minimises the data imported
#' mydata <- readAntares(areas = "all", TODO = ...,
#'                       select = c("H. ROR", "WIND", "SOLAR", "MISC. NDG",
#'                                  "LOAD", "BALANCE"), mustRun = TRUE)
#' addNetLoad(mydata)
#' externalDependancies(mydata)
#' }
#'
#' @export
#'
externalDependancies <- function(x , timeStep = "annual", synthesis = FALSE) {

  # Check that x contains is a antaresDataList
  if (is(x, "antaresDataList")) {
    # Check that x contains the needed variables
    if(is.null(x$areas) & is.null(x$districts)) stop("'x' has to contain 'area' and/or 'district' data")
    if (!is.null(x$areas)) externalDependancies(x$areas,timeStep,synthesis)
    if (!is.null(x$districts)) externalDependancies(x$districts,timeStep,synthesis)
  }

  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  #check if x have the
  x <- .checkAttrs(x, timeStep = "hourly")

  neededCol<-list()
  if (!is.null(x$areas)) {
    neededCol$areas <- .neededColAreaExternalDepandancies
  }

  if (!is.null(x$districts)) neededCol$districts <- .neededColAreaExternalDepandancies

  x <- .checkColumns(x, neededCol)

  opts <- simOptions(x)

  if (!is.null(x$areas)) idVars <- .idCols(x$areas)
  else {
    idVars <- .idCols(x$districts)
    idVars[idVars == "district"] <- "area"
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

    if(!is.null(x$areas)){
      x <- .checkColumns(x, list(areas = c("pumpingCapacity", "storageCapacity")))
    }

    if(!is.null(x$districts)){
      x <- .checkColumns(x, list(districts = c("pumpingCapacity", "storageCapacity")))
    }

  }

  # Effective computation of the externalDepancies. The function .computeExternalDependancies is
  # defined below.
  res <- list()

  if (!is.null(x$areas)) {
    res$areas <- .computeExternalDependancies(x$areas, timeStep, synthesis)
    attr(res$areas, "type") <- "areaExternalDependancies"
  }

  if (!is.null(x$districts)) {
    res$districts <- .computeExternalDependancies(x$districts, timeStep, synthesis)
    attr(res$districts, "type") <- "districtExternalDependancies"
  }

  .addClassAndAttributes(res, attr(x, "synthesis"), attr(x, "timeStep"), opts, simplify = TRUE)
}



#' Private function used in function "externalDependancies".
#'
#' @param dataInput
#'   an antaresDataTable object containing areas or districts.
#'
#' @noRd
.computeExternalDependancies <- function(dataInput, timeStep, synthesis) {
  idVars <- .idCols(dataInput)

  # Create the main table that will be used to compute the margins
  data <- dataInput[, c(idVars, .neededColAreaExternalDepandancies), with = FALSE]

  #if we don't have pumpingCapacity and storageCapacity, we add columns empty
  if(is.null(data$pumpingCapacity)){
    data[ , c("pumpingCapacity", "storageCapacity") := 0]
  }
  # Compute externalDependanciesLevel
  data[,`:=`(
    exportsLevel = netLoad + pumpingCapacity,
    importsLevel = netLoad - `AVL DTG` - hstorPMaxAvg - storageCapacity
  )]
  # Compute externalDependanciesFrequency
  #init
  data[,`:=`(
    exportsFrequency = 0,
    importsFrequency = 0
  )]

  #compute
  data[importsLevel > 0 ,`:=`(importsFrequency = 1)]
  data[exportsLevel < 0 ,`:=`(exportsFrequency = 1)]

  data<-data[, c(idVars, "exportsLevel", "importsLevel", "exportsFrequency", "importsFrequency" ),with = FALSE]

  data <- changeTimeStep(data, timeStep, "hourly")

  if (synthesis) data <- synthesize(data)

  data

}
