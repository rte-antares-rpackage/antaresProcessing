#Copyright © 2016 RTE Réseau de transport d’électricité

.neededColAreaExternalDepandancies <- c("netLoad", "AVL DTG", "hstorPMaxAvg")

#' External Dependencies in imports and exports
#'
#' This function computes the dependency in imports and export for each area or districts at a
#' given time step. Dependency in imports represents moments where imports are required
#' to have no loss of load. Depandency in exports represents moments where exports are required to
#' have no spilled energy.
#'
#' @param x
#'   An object created with function \code{\link[antaresRead]{readAntares}}. It
#'   must contain data for areas and/or distritcs . More specifically this
#'   function requires the columns \code{hstorPMaxAvg}, and \code{netLoad}. To
#'   get these columns, one has to invoke \code{\link[antaresRead]{readAntares}}
#'   with the parameter \code{hydroStorageMaxPower = TRUE} and
#'   \code{\link[antaresProcessing]{addNetLoad}} (see examples).
#'
#'   Moreover it needs to have a hourly time step.
#'
#'   This object must also contain linkCapacity if there was virtual areas
#'   remove by \code{\link[antaresRead]{removeVirtualAreas}} to be able to
#'   calculate pumping and storage capacities.
#' @param timeStep
#'   Desired time step for the result.
#' @param synthesis
#'   If TRUE, average external dependncies are returned. Else the function
#'   returns external dependncies per Monte-Carlo scenario.
#' @param opts opts
#'
#' @return
#' A data.table of class \code{antaresDataTable} with the following columns:
#' \item{area}{Area name.}
#' \item{timeId}{Time id and other time columns.}
#' \item{pumping}{capacity of pumping}
#' \item{storage}{capacity of storage}
#' \item{exportsLevel}{netLoad + pumping}
#' \item{importsLevel}{netLoad - `AVL DTG` - hydroStorageMaxPower - storage > 0}
#' \item{exportsFrequency}{number of time step where this criteria is satisfied
#'
#'   criteria : netLoad + pumping < 0
#' }
#' \item{importsFrequency}{number of time step where this criteria is satisfied
#'
#'   criteria : netLoad - `AVL DTG` - hydroStorageMaxPower - storage > 0
#' }
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#' showAliases("externalDependency")
#'
#' mydata <- readAntares(select = "externalDependency")
#' addNetLoad(mydata)
#' externalDependency(mydata)
#'
#' # if there are some virtual pumping/storage areas, remove them with
#' # removeVirtualAreas
#' mydata <- removeVirtualAreas(mydata, c("pumping", "storage"))
#' externalDependency(mydata, ignoreMustRun = TRUE)
#' }
#'
#' @export
#'
externalDependency <- function(x , timeStep = "annual", synthesis = FALSE, opts = NULL) {

  # Check that x contains is a antaresDataList
  if (is(x, "antaresDataList")) {
    # Check that x contains the needed variables
    if(is.null(x$areas) & is.null(x$districts)) stop("'x' has to contain 'area' and/or 'district' data")
    if (!is.null(x$areas)) externalDependency(x$areas,timeStep,synthesis)
    if (!is.null(x$districts)) externalDependency(x$districts,timeStep,synthesis)
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

  if(is.null(opts))
  {
  opts <- simOptions(x)
  }

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

  # Effective computation of the externalDepancies. The function .computeexternalDependency is
  # defined below.
  res <- list()

  if (!is.null(x$areas)) {
    res$areas <- .computeexternalDependency(x$areas, timeStep, synthesis)
    attr(res$areas, "type") <- "areaexternalDependency"
  }

  if (!is.null(x$districts)) {
    res$districts <- .computeexternalDependency(x$districts, timeStep, synthesis)
    attr(res$districts, "type") <- "districtexternalDependency"
  }

  .addClassAndAttributes(res, attr(x, "synthesis"), attr(x, "timeStep"), opts, simplify = TRUE)
}



#' Private function used in function "externalDependency".
#'
#' @param dataInput
#'   an antaresDataTable object containing areas or districts.
#'
#' @noRd
.computeexternalDependency <- function(dataInput, timeStep, synthesis) {
  idVars <- .idCols(dataInput)

  # Create the main table that will be used to compute the margins
  data <- dataInput[, c(idVars, .neededColAreaExternalDepandancies), with = FALSE]

  #if we don't have pumpingCapacity and storageCapacity, we add columns empty
  if(is.null(data$pumpingCapacity)){
    data[ , c("pumpingCapacity", "storageCapacity") := 0]
  }
  # Compute externalDependencyLevel
  data[,`:=`(
    exportsLevel = netLoad + pumpingCapacity,
    importsLevel = netLoad - `AVL DTG` - hstorPMaxAvg - storageCapacity
  )]
  # Compute externalDependencyFrequency
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
