#Copyright © 2016 RTE Réseau de transport d’électricité

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
#' \code{addLoadFactorLink} modifies its input by adding to it two columns:
#'
#' \item{loadFactor}{Proportion of the installed capacity of a link that
#' is effectively used:
#'
#' \preformatted{loadFactor = `FLOW LIN` / transCapacity}
#'
#' Notice that \code{loadFactor} can be positive or negative according to the
#' direction of the flow.
#' }
#' \item{congestion}{1 if the link is saturated (\code{loadFactor = +/-1)},
#' 0 otherwise.
#' }
#' For convenience, the function invisibly returns the modified input.
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
    return(invisible(x))
   }

  #check when x is an antaresData
  if(! attr(x, "type") %in% c("links")  ) stop("'x' does not contain link data")

  missingCols <- setdiff(c("FLOW LIN.", "transCapacityDirect", "transCapacityIndirect"),
                         names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  x[ `FLOW LIN.`>0 & transCapacityDirect != 0, `:=` (
    loadFactor=as.double(`FLOW LIN.`/transCapacityDirect),
    congestion=ifelse(`FLOW LIN.`/transCapacityDirect >= 1, 1L, 0L)
    )]

  x[ `FLOW LIN.`<0 & transCapacityIndirect != 0, `:=` (
    loadFactor=-as.double(-`FLOW LIN.`/transCapacityIndirect),
    congestion= ifelse(-`FLOW LIN.`/transCapacityIndirect >= 1, 1L, 0L)
    )]

  x[ `FLOW LIN.`==0 , `:=` (
    loadFactor=0,
    congestion=0
  )]


  invisible(x)
}
