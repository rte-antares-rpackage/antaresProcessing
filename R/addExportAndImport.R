#Copyright © 2016 RTE Réseau de transport d’électricité

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
#' @param opts opts
#'
#' @return
#' \code{addExportAndImport} modifies its input by adding to it columns:
#'
#' \item{export}{export for an area or district}
#' \item{import}{import for an area or district}
#' \item{capExport}{capacity of export for an area or district, if \code{addCapacities} is set to TRUE}
#' \item{capImport}{capacity of import for an area or district, if \code{addCapacities} is set to TRUE}
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#' showAliases("exportsImports")
#'
#' mydata <- readAntares(select = "exportsImports")
#' addExportAndImport(mydata)
#' names(mydata$areas)
#'
#' }
#' @export
#'

addExportAndImport <- function(x, addCapacities = FALSE, opts = NULL) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  if (is(x, "antaresDataList")) {

    # Check that necessary links are present in the object
    areas <- unique(x$areas$area)
    vnodes <- unlist(attr(x, "virtualNodes"))

    if(is.null(opts))
    {
    opts <- simOptions(x)
    }
    neededLinks <- getLinks(areas, exclude = vnodes, opts = opts)

    links <- unique(x$links$link)
    missingLinks <- setdiff(neededLinks, links)
    if (length(missingLinks) > 0) stop("The following links are needed but missing: ",
                                       paste(missingLinks, collapse = ", "))

    if (!is.null(x$areas)) .addExportImportForArea(x$areas, x$links, neededLinks, addCapacities)
    if (!is.null(x$district)) .addExportImportForArea(x$districts, x$links, neededLinks, addCapacities)
    invisible(x)
  }

}

.addExportImportForArea <- function(dataAreas, dataLinks, neededLinks, addCapacities) {

  opts <- simOptions(dataAreas)
  idColsL <- .idCols(dataLinks)
  idColsA <- .idCols(dataAreas)

  if (!is.null(dataAreas$export)) {
    stop("Input already contains column 'export' and 'import' ")
  }

  # Get the links needed to compute exchanges
  links <- getLinks(opts = opts, namesOnly = FALSE, withDirection = TRUE)
  links <- links[link %in% neededLinks]

  if (!is.null(dataAreas$district)) {
    # Replace areas by districts in links
    links[opts$districtsDef, district := district, on=c(area = "area")]
    links[opts$districtsDef, toDistrict := district, on=c(to = "area")]

    # Remove links connecting two areas of the same district
    links <- links[!is.na(district)]
    links <- links[is.na(toDistrict) | district != toDistrict]
  }

  #get the values of flow and capacities if needed
  if(addCapacities){

    if("transCapacityIndirect" %in% names(dataLinks)) {
      flowLinks<-dataLinks[, c(idColsL, "FLOW LIN.", "transCapacityDirect", "transCapacityIndirect"),
                           with = FALSE]
    }else{
      stop("x does not contain transCapacityDirect or transCapacityIndirect data ")
    }

  }else{
    flowLinks <- dataLinks[, c(idColsL, "FLOW LIN."), with = FALSE]
  }

  flowLinks <- flowLinks[links, on= c(link="link"), allow.cartesian = TRUE]
  flowLinks[, `:=`(
    export = pmax(0, direction * `FLOW LIN.`),
    import = pmax(0, - direction * `FLOW LIN.`)
  )]

  if (addCapacities) {
    flowLinks[, `:=`(
      capExport = ifelse(direction == 1, transCapacityDirect, transCapacityIndirect),
      capImport = ifelse(direction == -1, transCapacityIndirect, transCapacityDirect)
    )]
  }

  # Names of the columns to add
  v <- c("import", "export")
  if (addCapacities) v <- c(v, "capExport", "capImport")

  # Aggregate flows and capacities by area (or district)
  flowAreas <- flowLinks[, lapply(.SD, sum), keyby = idColsA, .SDcols = v]

  # Add columns by reference to the input data
  setkeyv(dataAreas, idColsA)
  dataAreas[flowAreas, c(v) := mget(v)]
}
