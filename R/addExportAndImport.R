#' Export and import of areas or districts
#'
#' This function computes the export and import of areas or districts and add it to an
#' \code{antaresData} object.
#'
#'@param x
#'   an object of class "antaresDataList" created with the function
#'   \code{readAntares}. It has to contain some areas and all the links that are
#'   connected to these areas. Moreover the function \code{\link{removeVirtualAreas}} must be call before.
#' @param addCapacities
#'   If \code{TRUE}, export and import capacities are added.
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", links="all", districts ="all" , synthesis = FALSE)
#'
#' mydata<-removeVirtualAreas(mydata, storageFlexibility = c(getAreas("psp"),getAreas("hub")), production = getAreas("off") )
#'
#' mydata<-addExportAndImport(mydata)
#'
#' names(mydata$areas)
#' names(mydata$districts)
#'
#' }
#' @export
#'

addExportAndImport <- function(x, addCapacity = FALSE) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  if (is(x, "antaresDataList")) {

    # Check that necessary links are present in the object
    areas <- unique(x$areas$area)
    vnodes <- unlist(attr(x, "virtualNodes"))

    opts <- simOptions(x)
    neededLinks <- getLinks(areas, exclude = vnodes, opts = opts)

    links <- unique(x$links$link)
    missingLinks <- setdiff(neededLinks, links)
    if (length(missingLinks) > 0) stop("The following links are needed but missing: ",
                                       paste(missingLinks, collapse = ", "))

    if (!is.null(x$areas)) x<-.addExportImportForArea(x, neededLinks, FALSE,addCapacity)
    if (!is.null(x$districts)) x<-.addExportImportForDistrict(x, neededLinks,addCapacity)
    return(x)
  }

}

.addExportImportForArea<- function(x, neededLinks, bool, addCapacity) {

  dataAreas<-x$areas
  dataLinks<-x$links
  CalledByAddExportImportForDistrict<-bool

  if(! attr(dataAreas, "type") %in% c("areas", "links")) stop("'x' does not contain area or links data")

  if (!is.null(dataAreas$export) & !CalledByAddExportImportForDistrict) {
    stop("Input already contains column 'export' and 'import' ")
  }

  # get the direction of links
  linksDirection <- tstrsplit(neededLinks, split = " - ")
  linksDirection <- data.table(link = neededLinks, from = linksDirection[[1]], to = linksDirection[[2]])

  #get the values of flow and capacities if needed
  idColsL <- .idCols(dataLinks)
  if(addCapacity){

    if("transCapacityIndirect" %in% names(dataLinks)) {
      flowLinks<-dataLinks[, .(`FLOW LIN.`, transCapacityDirect, transCapacityIndirect), by=idColsL]
    }else{
      stop("x does not contain transCapacityDirect or transCapacityIndirect data ")
    }

  }else{
    flowLinks <- dataLinks[, .(`FLOW LIN.`), by=idColsL]
  }

  flowLinks <- merge(linksDirection, flowLinks, by = "link", allow.cartesian = TRUE)
  flowLinks[`FLOW LIN.`>0 , ':=' (exportFrom=as.double(`FLOW LIN.`), importFrom=as.double(0),exportTo=as.double(0), importTo=as.double(`FLOW LIN.`) )]
  flowLinks[`FLOW LIN.`<0 , ':=' (exportFrom=as.double(0), importFrom=as.double(-`FLOW LIN.`),exportTo=as.double(-`FLOW LIN.`), importTo=as.double(0) )]
  flowLinks[`FLOW LIN.`==0 , ':=' (exportFrom=as.double(0), importFrom=as.double(0), exportTo=as.double(0), importTo=as.double(0) )]

  #get the values for export and import when the area is "from"
  flowLinksFrom<-copy(flowLinks)
  #delete not important data
  flowLinksFrom[ , c("exportTo", "importTo"):=NULL]
  setnames(flowLinksFrom, "from", "area")
  setnames(flowLinksFrom, "exportFrom", "export")
  setnames(flowLinksFrom, "importFrom", "import")


  if(addCapacity){
    flowLinksFrom<-flowLinksFrom[ , .(export=sum(export),import=sum(import), capExport=sum(transCapacityDirect), capImport=sum(transCapacityIndirect)), by=eval(.idCols(dataAreas))]
  }else{
    flowLinksFrom<-flowLinksFrom[ , .(export=sum(export),import=sum(import)), by=eval(.idCols(dataAreas))]
  }

  flowLinksFrom[ , c("link", "to", "`FLOW LIN.`"):=NULL]

  #get the values for export and import when the area is "to"
  flowLinksTo<-copy(flowLinks)
  #delete not important data
  flowLinksTo[ , c("exportFrom", "importFrom"):=NULL]
  setnames(flowLinksTo, "to", "area")
  setnames(flowLinksTo, "exportTo", "export")
  setnames(flowLinksTo, "importTo", "import")

  if(addCapacity){
    flowLinksTo<-flowLinksTo[ , .(export=sum(export),import=sum(import), capExport=sum(transCapacityIndirect), capImport=sum(transCapacityDirect)), by=eval(.idCols(dataAreas))]
  }else{
    flowLinksTo<-flowLinksTo[ , .(export=sum(export),import=sum(import)), by=eval(.idCols(dataAreas))]
  }

  flowLinksTo[ , c("link", "to", "`FLOW LIN.`"):=NULL]

  #merge the data import and export
  flowLinksWithFromAndTo<-merge(flowLinksFrom,flowLinksTo, all=TRUE, by=.idCols(dataAreas), sort=TRUE)
  flowLinksWithFromAndTo[is.na(export.x), export.x:=0]
  flowLinksWithFromAndTo[is.na(export.y), export.y:=0]
  flowLinksWithFromAndTo[is.na(import.x), import.x:=0]
  flowLinksWithFromAndTo[is.na(import.y), import.y:=0]

  flowLinksWithFromAndTo<-flowLinksWithFromAndTo[,':=' (export=export.x+export.y, import=import.x+import.y)]
  flowLinksWithFromAndTo[ , c("export.x", "export.y", "import.x","import.y" ):=NULL]

  if(addCapacity){
    flowLinksWithFromAndTo[is.na(capExport.x), capExport.x:=0]
    flowLinksWithFromAndTo[is.na(capExport.y), capExport.y:=0]
    flowLinksWithFromAndTo[is.na(capImport.x), capImport.x:=0]
    flowLinksWithFromAndTo[is.na(capImport.y), capImport.y:=0]
    flowLinksWithFromAndTo<-flowLinksWithFromAndTo[,':=' (capExport=capExport.x+capExport.y, capImport=capImport.x+capImport.y)]
    flowLinksWithFromAndTo[ , c("capExport.x", "capExport.y", "capImport.x","capImport.y" ):=NULL]
  }

  #merge with the data of an area
  dataAreas <- merge(dataAreas, flowLinksWithFromAndTo, by = .idCols(dataAreas))

  #when the fonction is call twice (like for districts) we must deleted the previous result
  if("export.x" %in% names(dataAreas) | "import.x" %in% names(dataAreas)  ){
    dataAreas<-dataAreas[,':=' (export=export.y, import=import.y)]
    dataAreas[ , c("export.x", "export.y", "import.x","import.y" ):=NULL]

    #we must to the same for values capacities
    if(addCapacity){
      dataAreas<-dataAreas[,':=' (capExport=capExport.y, capImport=capImport.y)]
      dataAreas[ , c("capExport.x", "capExport.y", "capImport.x","capImport.y" ):=NULL]
    }
  }
  x$areas<-dataAreas

  x

}

.addExportImportForDistrict<- function(x, neededLinks, addCapacity) {

  if (!is.null(x$districts$export)) {
    stop("Input already contains column 'export' and 'import' ")
  }

  if(! attr(x$districts, "type") %in% c("districts")) stop("'x' does not contain districts data")

  #get the districts and areas
  opts <- simOptions(x)
  districts <- intersect(unique(x$districts$district), opts$districtsDef$district)
  districtsDef <- split(opts$districtsDe$area, opts$districtsDef$district)

  #delete links if they are internal to a district
  #get links not internal to a district
  ListInternalDistrict <- vector()
  i=1
  for(d in districts){
    ListInternalDistrict[i]<-getLinks(area=districtsDef[d][[1]], internalOnly = TRUE)
    LinksWithOutInternalLinksDistrict<-setdiff(neededLinks, ListInternalDistrict[i])

    #delete links not rattached in an area of the district
    # get the direction of links
    linksDirection <- tstrsplit(LinksWithOutInternalLinksDistrict, split = " - ")
    linksDirection <- data.table(link = LinksWithOutInternalLinksDistrict, from = linksDirection[[1]], to = linksDirection[[2]])
    linksDirection<- linksDirection[ from %in% districtsDef[d][[1]] | to %in% districtsDef[d][[1]] , ]

    #get the needed links for the district
    neededLinksForADistrict<-linksDirection$link

    #get the values of export and import of areas (without implication of links internal)
    copyForDistrict<-copy(x)
    copyForDistrict<-.addExportImportForArea(copyForDistrict, neededLinksForADistrict, TRUE, addCapacity)

    if(addCapacity){
      dataExportImportArea<-copyForDistrict$areas[,.(export=export, capExport=capExport, import=import, capImport=capImport), by=c(.idCols(copyForDistrict$areas))]
    }else{
      dataExportImportArea<-copyForDistrict$areas[,.(export=export, import=import), by=c(.idCols(copyForDistrict$areas))]
    }

    #get the values agreged for districts
    valueForDistrictsInt<-.groupByDistrict(dataExportImportArea,opts)

    #get the values only for the district that concerns us
    valueForDistrict<-valueForDistrictsInt[district==d, ]

    #get the values for all distritcs
    if(d==districts[[1]]) {valueForAllDistricts<-valueForDistrict}
    valueForAllDistricts<-rbind(valueForDistrict,valueForAllDistricts)

    i=i+1
  }

  x$districts <- merge(x$districts, valueForAllDistricts, by = .idCols(x$districts))

  #remove duplicated row
  x$districts<-unique(x$districts)

  x
}
