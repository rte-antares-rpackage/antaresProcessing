#Copyright © 2016 RTE Réseau de transport d’électricité

setAlias(
  "loadFactorLink",
  "Data required by 'addLoadFactorLink()'",
  c("links", "FLOW LIN.", "linkCapacity")
)

#' Load factors of link
#'
#' This function computes the load factor of link and add it to an
#' \code{antaresData} object.
#'
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It must contain the columns
#'   \code{transCapacityDirect} and \code{transCapacityIndirect}.
#'
#' @return
#' \code{addLoadFactorLink} modifies its input by adding to it a column "loadFactor". For
#' convenience, it invisibly returns the modified input.
#'   loadFactor represent the proportion of
#'   the installed capacity of a link that is effectively used
#'
#'   Formula: `FLOW LIN` / transCapacity
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#' showAliases("loadFactorLink")
#'
#' mydata <- readAntares(select = "loadFactorLink")
#' addLoadFactorLink(mydata)
#' names(mydata)
#' }
#'
#' @export
#'
addLoadFactorLink <- function(x) {

  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  #check when x is an antaresDataList
   if (is(x, "antaresDataList")) {
    if(! c("links") %in% attr(x, "names")) stop("'x' does not contain link data")
    if (!is.null(x$links)) addLoadFactorLink(x$links)
    return("loadFactorLink added")
   }

  #check when x is an antaresData
  if(! attr(x, "type") %in% c("links")  ) stop("'x' does not contain link data")

  if (!is.null(x$loadFactor)) {
    stop("Input already contains column 'netLoad'")
  }

  missingCols <- setdiff(c("FLOW LIN.", "transCapacityDirect", "transCapacityIndirect"),
                         names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  x[ `FLOW LIN.`>0 & transCapacityDirect != 0, `:=` (
    loadFactor=as.double(`FLOW LIN.`/transCapacityDirect),
    congestion=as.integer(`FLOW LIN.`/transCapacityDirect)
    )]

  x[ `FLOW LIN.`<0 & transCapacityIndirect != 0, `:=` (
    loadFactor=-as.double(-`FLOW LIN.`/transCapacityIndirect),
    congestion=as.integer(-`FLOW LIN.`/transCapacityIndirect)
    )]

  x[ `FLOW LIN.`==0 , `:=` (
    loadFactor=0,
    congestion=0
  )]


  invisible(x)
}
