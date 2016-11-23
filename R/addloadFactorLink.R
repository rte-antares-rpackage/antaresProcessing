#Copyright © 2016 RTE Réseau de transport d’électricité

#' Load factors of link
#'
#' This function computes the load factor of link and add it to an
#' \code{antaresData} object.
#'
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It must contain hourly detailed
#'   results for link and has to contain the columns
#'   \code{transCapacityDirect} and \code{transCapacityIndirect}.
#'
#' @return
#' \code{addLoadFactorLink} modifies its input by adding to it a column "loadFactorLink". For
#' convenience, it invisibly returns the modified input.
#'   loadFactorLink represent the proportion of
#'   the installed capacity of a link that is effectively used\cr
#'   Formula: `FLOW LIN` / transCapacity
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(link = "all", linkCapacity=TRUE)
#'
#' addLoadFactorLink(mydata)
#' names(mydata)
#'
#' # Example that minimizes the quantity of data read
#' mydata <- readAntares(links = "all", timeStep = "annual",select = c("FLOW LIN."), linkCapacity=TRUE)
#' addLoadFactorLink(mydata)
#' }
#'
#' @export
#'
addLoadFactorLink <- function(x) {

  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

   if (is(x, "antaresDataList")) {
    if(! c("links") %in% attr(x, "names")) stop("'x' does not contain link data")
    if (!is.null(x$links)) addLoadFactorLink(x$links)
    return("loadFactorLink added")
   }

  if(! attr(x, "type") %in% c("links")  ) stop("'x' does not contain link data")

  if (!is.null(x$addLoadFactorLink)) {
    stop("Input already contains column 'netLoad'")
  }

  missingCols <- setdiff(c("FLOW LIN.", "transCapacityDirect", "transCapacityIndirect"),
                         names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  x[ `FLOW LIN.`>0 & transCapacityDirect != 0, loadFactorLink:=as.integer(`FLOW LIN.`/transCapacityDirect)]

  x[ `FLOW LIN.`<0 & transCapacityIndirect != 0, loadFactorLink:=-as.integer(`FLOW LIN.`/transCapacityIndirect)]

  x[ `FLOW LIN.`==0 , loadFactorLink:=0]


  invisible(x)
}
