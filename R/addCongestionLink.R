#Copyright © 2016 RTE Réseau de transport d’électricité

#' Add the congestion frequency and the number of congested hours for a given link
#'
#' This function computes 4 congestion variables of link (congestion frequency and congestion hours in direct and indirect direction) and adds them to an
#' \code{antaresData} object. The input object must be at an hourly timestep.
#'
#' @param x
#'   Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. It must contain the columns
#'   \code{CONG. PROB +} and \code{CONG. PROB -} and be at an hourly timestep.
#' @param timeStep \code{character} Desired time step for the result.
#'
#' @return
#' \code{addCongestionLink} modifies its input by adding four columns:
#'
#' \item{congestionFrequencyDirect}{
#'
#' This is the congestion frequency on the direct direction of the link at the specified time resolution.
#' \preformatted{congestionFrequencyDirect = round(sum((`CONG. PROB +` != 0)/.N), 2)}
#'
#' }
#'
#' \item{congestionFrequencyIndirect }{
#'
#' This is the congestion frequency on the indirect direction of the link at the specified time resolution.
#' \preformatted{congestionFrequencyIndirect  = round(sum((`CONG. PROB -` != 0)/.N), 2)}
#'
#' }
#'
#' \item{congestionHoursDirect}{
#'
#' This is the number of congestion hours on the direct direction of the link at the specified time resolution.
#' \preformatted{congestionHoursDirect  = sum(`CONG. PROB +` != 0)}
#'
#' }
#'
#' \item{congestionHoursIndirect }{
#'
#' This is the number of congestion hours on the direct direction of the link at the specified time resolution.
#' \preformatted{congestionHoursIndirect =  sum(`CONG. PROB -` != 0)}
#'
#' }
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#'
#' mydata <- readAntares(links = "all")
#' mydata <- addCongestionLink(mydata, timeStep = "daily")
#' names(mydata)
#'
#' mydata <- addCongestionLink(mydata, timeStep = c('daily'))
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
