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
#' @param mcyear
#'  set of mcYear
#'
#' @examples
#' \dontrun{
#'
#' mydata <- readAntares(areas="all",clusters="all", select="LOAD")
#' getValues(mydata$areas, variable="LOAD")
#' getValues(myData$clusters, variable = "production")
#'
#' }
#' @export
#'

getValues<-function(data=NULL, variable=NULL, mcyear="all"){

  if (!is(data, "antaresData")) stop("'data' is not an 'antaresData' object")
  if (is.null(variable)) stop("'variable' is NULL")
  if (!is.character(variable)) stop("'variable' is not a character")
  if (!is.numeric(mcyear) & mcyear!="all") stop("'mcyear' is not a numeric")

  IdCols<-getIdCols(data)
  #without mcYear
  IdColsWM<-IdCols[IdCols!="mcYear"]
  toGet<-c(IdCols,variable)

  res<-data[, toGet, with=FALSE ]
  if(mcyear!="all") res<-res[mcYear %in% mcyear]

  myFormula<-sprintf("%s ~ mcYear",paste(IdColsWM, collapse = "+") )
  res<-dcast(data = res, as.formula(myFormula), value.var=variable)
  res

}
