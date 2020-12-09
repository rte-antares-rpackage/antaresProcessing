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
#' @param timestep \code{character} timestep for aggregation.
#'
#' @return
#' \code{addLoadFactorLink}
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#'
#' mydata <- readAntares(links = "all")
#' mydata <- addCongestionLink(mydata, timestep = "daily")
#' names(mydata)
#'
#' mydata <- addCongestionLink(mydata, timestep = c('daily'))
#' }
#'
#' @export
#'
addCongestionLink <- function(x, timestep = "daily") {

  if(!timestep %in% c("weekly", "daily", "monthly", "annual")){
    stop("timestep must be weekly, daily, monthly or annual")
  }

  if(timestep == "weekly"){
    x$wd <- data.table::week(as.Date(x$time))
    by = c("wd", "link")
  }


  if(timestep == "daily"){
    by = c("link", "day", "month")
  }
  if(timestep == "monthly"){
    by = c("link", "month")
  }

  if(timestep == "annual"){
    by = c("link")
  }


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

  # byVect <-  getIdCols(x)[getIdCols(x) %in% c("link")]
  # byVect <- c(byVect, by)

  if(!all(by %in% names(x))){
    bymiss <- by[!by %in% names(x)]
    bymiss <- paste0(bymiss, collapse = ",")
    stop(paste0("All columns specify in by must be in link data : ", bymiss))
  }

  x <- merge(x, unique(x[, .(congestionFrequencyDirect = round(sum((`CONG. PROB +` != 0)/.N), 2),
        congestionFrequencyIndirect = round(sum((`CONG. PROB -` != 0)/.N), 2),
        congestionHoursDirect = sum(`CONG. PROB +` != 0),
        congestionHoursIndirect = sum(`CONG. PROB -` != 0)
        ), by = by]), by = by)
  if(timestep == "weekly")x$wd <- NULL
  invisible(x)
}
