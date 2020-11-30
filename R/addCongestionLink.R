#Copyright © 2016 RTE Réseau de transport d’électricité

#' Load factors of link
#'
#' This function computes the load factor of link and add it to an
#' \code{antaresData} object.
#'
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It must contain the columns
#'   \code{CONG. PROB +} and \code{CONG. PROB -}.
#'
#' @return
#' \code{addLoadFactorLink}
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#'
#' mydata <- readAntares(links = "all")
#' mydata <- addCongestionLink(mydata)
#' names(mydata)
#' }
#'
#' @export
#'
addCongestionLink <- function(x) {

  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  #check when x is an antaresDataList
  if (is(x, "antaresDataList")) {
    if(! c("links") %in% attr(x, "names")) stop("'x' does not contain link data")
    if (!is.null(x$links)) x$links <- addCongestionLink(x$links)
    return(invisible(x))
  }

  #check when x is an antaresData
  if(! attr(x, "type") %in% c("links")  ) stop("'x' does not contain link data")

  missingCols <- setdiff(c("CONG. PROB +", "CONG. PROB -"),
                         names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  byVect <-  getIdCols(x)[getIdCols(x) %in% c("link", "mcYear")]

  x <- merge(x, x[, .(congestionFrequencyDirect = round(sum((`CONG. PROB +` != 0)/.N), 2),
        congestionFrequencyIndirect = round(sum((`CONG. PROB -` != 0)/.N), 2),
        congestionHoursDirect = sum(`CONG. PROB +` != 0),
        congestionHoursIndirect = sum(`CONG. PROB -` != 0)
        ), by = byVect])
  invisible(x)
}
