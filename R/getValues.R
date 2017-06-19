#Copyright © 2016 RTE Réseau de transport d’électricité

#' Get values of a variable
#'
#' Get all the values of a variable for some years Monte Carlo
#'
#'@param data
#'   an object of class "antaresData" created with the function
#'   \code{readAntares}.
#'
#' @param variable
#'   a variable of data
#'
#' @param mcYear
#'  set of mcYear
#'
#' @examples
#' \dontrun{
#'
#' mydata <- readAntares(areas="all", select="LOAD")
#' getVariableAntares(mydata, variable="LOAD")
#' names(mydata$areas)
#'
#' }
#' @export
#'

getValues<-function(data=NULL, variable=NULL, mcYear="all"){

  if (!is(data, "antaresData")) stop("'data' is not an 'antaresData' object")
  if (is.null(variable)) stop("'variable' is NULL")
  if (is.null(variable)) stop("'variable' is not a character")
  if (is.null(mcYear)) stop("'mcYear' is not a numeric")

  TRUE
}
