#Copyright © 2016 RTE Réseau de transport d’électricité

#' Add congestion of link
#'
#' This function computes 4 congestion variables of link and add it to an
#' \code{antaresData} object. It's only be done on hourly data.
#'
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It must contain the columns
#'   \code{CONG. PROB +} and \code{CONG. PROB -}.
#' @param timestep \code{character} Desired time step for the result.
#'
#' @return
#' \code{addCongestionLink} modifies its input by adding four columns:
#'
#' \item{congestionFrequencyDirect}{
#'
#' \preformatted{congestionFrequencyDirect = round(sum((`CONG. PROB +` != 0)/.N), 2)}
#'
#' }
#'
#' \item{congestionFrequencyIndirect }{
#'
#' \preformatted{congestionFrequencyIndirect  = round(sum((`CONG. PROB -` != 0)/.N), 2)}
#'
#' }
#'
#' \item{congestionHoursDirect}{
#'
#' \preformatted{congestionHoursDirect  = sum(`CONG. PROB +` != 0)}
#'
#' }
#'
#' \item{congestionHoursIndirect }{
#'
#' \preformatted{congestionHoursIndirect =  sum(`CONG. PROB -` != 0)}
#'
#' }
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
addCongestionLink <- function(x,
                              timeStep = c("daily", "weekly", "monthly", "annual")) {

  if(! "hourly" %in% attr(x, "timeStep")){
    stop("'addCongestionLink' can be used only on 'hourly' data")
  }

  timeStep <- match.arg(timeStep)

  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  # check when x is an antaresDataList
  if (is(x, "antaresDataList")) {
    if(! c("links") %in% attr(x, "names")) stop("'x' does not contain link data")
    if (!is.null(x$links)) x$links <- addCongestionLink(x$links, timeStep = timeStep)
    return(invisible(x))
  }

  # check when x is an antaresData
  if(! attr(x, "type") %in% c("links")  ) stop("'x' does not contain link data")

  if(timeStep == "daily"){
    by = c("link", "day", "month")
  } else if(timeStep == "weekly"){
    x$wd <- data.table::week(as.Date(x$time))
    by = c("link", "wd")
  } else if(timeStep == "monthly"){
    by = c("link", "month")
  } else if(timeStep == "annual"){
    by = c("link")
  }

  if("mcYear" %in% names(x)){
    by = c(by, "mcYear")
  }
  missingCols <- setdiff(c("CONG. PROB +", "CONG. PROB -"), names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  x[, c("congestionFrequencyDirect", "congestionFrequencyIndirect", "congestionHoursDirect", "congestionHoursIndirect") := list(
    round(sum((`CONG. PROB +` != 0)/.N), 2),
    round(sum((`CONG. PROB -` != 0)/.N), 2),
    sum(`CONG. PROB +` != 0),
    sum(`CONG. PROB -` != 0)
  ), by = by]

  if(timeStep == "weekly") x$wd <- NULL
  invisible(x)
}
