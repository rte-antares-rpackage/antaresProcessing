#Copyright © 2016 RTE Réseau de transport d’électricité

#' Net load of areas
#'
#' This function computes the net load of areas or districts and add it to an
#' \code{antaresData} object. Net load is the load of an area minus productions
#' that are not controlled: wind, solar, hydraulic run of river, etc. the
#' production of clusters in must run mode is also subtracted by default.
#'
#' @param x
#'   An \code{antaresData} object created with readAntares. Unless
#'   \code{ignoreMustRun} is true, it must
#'   have a column  \code{mustRunTotal}.
#' @param ignoreMustRun
#'   If \code{TRUE}, the production in must run mode is not substracted to the
#'   net load.
#'
#' @return
#' \code{addNetLoad} modifies its input by adding to it a column "netLoad". For
#' convenience, it invisibly returns the modified input.
#'              formula = LOAD - `ROW BAL.` - PSP - `MISC. NDG` - `H. ROR` - WIND - SOLAR - mustRunTotal
#'
#' @examples
#' \dontrun{
#' # Data required by the function
#' showAliases("netLoad")
#'
#' mydata <- readAntares(select = "netLoad")
#' addNetLoad(mydata)
#' names(mydata)
#'
#' }
#'
#' @export
#'
addNetLoad <- function(x, ignoreMustRun = FALSE) {
  if (!is(x, "antaresData")) stop("'x' is not an 'antaresData' object")

  if (is(x, "antaresDataList")) {
    if (!is.null(x$areas)) addNetLoad(x$areas, ignoreMustRun)
    if (!is.null(x$districts)) addNetLoad(x$districts, ignoreMustRun)
    return(x)
  }

  if(! attr(x, "type") %in% c("areas", "districts")) stop("'x' does not contain area or district data")

  # if (!is.null(x$netLoad)) {
  #   stop("Input already contains column 'netLoad'")
  # }

  missingCols <- setdiff(c("LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND", "SOLAR"),
                         names(x))

  if (length(missingCols) > 0) {
    stop("The following columns are needed but missing: ", paste(missingCols, collapse = ", "))
  }

  if (ignoreMustRun) {

    x[,netLoad := LOAD - `ROW BAL.` - PSP - `MISC. NDG` - `H. ROR` - WIND - SOLAR]

  } else {

    if(is.null(x$mustRunTotal)){
      stop("Column 'mustRunTotal' is needed but missing. You can use argument 'ignoreMustRun=TRUE' if you do not want to take 'must run' production into account.")
    }
    x[, netLoad := LOAD - `ROW BAL.` - PSP - `MISC. NDG` - `H. ROR` - WIND - SOLAR - mustRunTotal]
  }

  invisible(x)
}
