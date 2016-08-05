#' Export and import of areas or districts
#'
#' This function computes the export and import of areas or districts and add it to an
#' \code{antaresData} object.
#'
#'@param x
#'   an object of class "antaresDataList" created with the function
#'   \code{readAntares}. It has to contain some areas and all the links that are
#'   connected to these areas. Moreover the function \code{\link{removeVirtualAreas}} must be call before.
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", links="all", districts ="all" , synthesis = FALSE)
#'
#' mydata<-removeVirtualAreas(mydata, storageFlexibility = c(getAreas("psp"),getAreas("hub")), production = getAreas("off") )
#'
#' mydata<-addExportImport(mydata)
#'
#' names(mydata$areas)
#' names(mydata$districts)
#'
#' }
#' @export
#'

addExportImport <- function(x) {
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

    if (!is.null(x$areas)) x<-.addExportImportForArea(x, neededLinks)
    if (!is.null(x$districts)) x<-..addExportImportForDistrict(x, neededLinks)
    return(x)
  }

}

.addExportImportForArea<- function(x, neededLinks) {

  dataAreas<-x$areas
  dataLinks<-x$links

  #TO DO
  if(! attr(dataAreas, "type") %in% c("areas", "links")) stop("'x' does not contain area or links data")

  if (!is.null(dataAreas$export)) {
    #stop("Input already contains column 'export' and 'import")
  }

  # get the direction of links
  linksDirection <- tstrsplit(neededLinks, split = " - ")
  linksDirection <- data.table(link = neededLinks, from = linksDirection[[1]], to = linksDirection[[2]])

  #get the values of flow
  idColsL <- .idCols(dataLinks)
  flowLinks <- dataLinks[, append(mget(idColsL), .(`FLOW LIN.`=`FLOW LIN.`))]

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
  flowLinksFrom<-flowLinksFrom[ , .(export=sum(export),import=sum(import)), by=eval(.idCols(dataAreas))]

  flowLinksFrom[ , c("link", "to", "`FLOW LIN.`"):=NULL]
  flowLinksFrom[ , `FLOW LIN.`:=NULL]

  #get the values for export and import when the area is "to"
  flowLinksTo<-copy(flowLinks)
  #delete not important data
  flowLinksTo[ , c("exportFrom", "importFrom"):=NULL]
  setnames(flowLinksTo, "to", "area")
  setnames(flowLinksTo, "exportTo", "export")
  setnames(flowLinksTo, "importTo", "import")
  flowLinksTo<-flowLinksTo[ , .(export=sum(export),import=sum(import)), by=eval(.idCols(dataAreas))]

  flowLinksTo[ , c("link", "to", "`FLOW LIN.`"):=NULL]
  flowLinksTo[ , `FLOW LIN.`:=NULL]

  #merge the data import and export
  flowLinksWithFromAndTo<-merge(flowLinksFrom,flowLinksTo, all=TRUE, by=.idCols(dataAreas), sort=TRUE)
  flowLinksWithFromAndTo[is.na(export.x), export.x:=0]
  flowLinksWithFromAndTo[is.na(export.y), export.y:=0]
  flowLinksWithFromAndTo[is.na(import.x), import.x:=0]
  flowLinksWithFromAndTo[is.na(import.y), import.y:=0]

  flowLinksWithFromAndTo<-flowLinksWithFromAndTo[,':=' (export=export.x+export.y, import=import.x+import.y)]
  flowLinksWithFromAndTo[ , c("export.x", "export.y", "import.x","import.y" ):=NULL]

  #merge with the data of an area
  dataAreas <- merge(dataAreas, flowLinksWithFromAndTo, by = .idCols(dataAreas))

  #when the fonction is call twice (like for districts) we must deleted the previous result
  if("export.x" %in% names(dataAreas) | "import.x" %in% names(dataAreas)  ){
    dataAreas<-dataAreas[,':=' (export=export.y, import=import.y)]
    dataAreas[ , c("export.x", "export.y", "import.x","import.y" ):=NULL]
  }
  x$areas<-dataAreas

  x

}

.addExportImportForDistrict<- function(x, neededLinks) {

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
    i=i+1
  }

  neededLinksForDistricts<-setdiff(neededLinks, ListInternalDistrict)

  #get the values of export and import of areas (without implication of links internal)
  copyForDistrict<-copy(x)
  copyForDistrict<-.addExportImportForArea(copyForDistrict, neededLinksForDistricts)

  dataExportImportArea<-copyForDistrict$areas[,.(export=export, import=import), by=c(.idCols(copyForDistrict$areas))]

  #get the values agreged by districts
  valueForDistrict<-.groupByDistrict(dataExportImportArea,opts)

  x$districts <- merge(x$districts, valueForDistrict, by = .idCols(x$districts))

  x
}
