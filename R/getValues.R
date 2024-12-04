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
#' @return A data.table, data.frame class object containing the study's output data
#'
#' @examples
#' \donttest{
#' library(antaresRead)
#' # with study test for example (study is in package antaresRead)
#' sourcedir <- system.file("testdata", package = "antaresRead")
#'
#' # untar study in temp dir
#' path_latest <- file.path(tempdir(), "latest")
#' untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path_latest)
#'
#' study_path <- file.path(path_latest, "test_case")
#'
#' # set path to your Antares simulation
#' opts <- setSimulationPath(study_path)
#'
#' # read output simulation
#' mydata <- readAntares(areas = "all",
#'                       links = "all",
#'                       clusters = "all",
#'                       timeStep = "annual",
#'                       mcYears = "all")
#'
#' # get values of a variable
#' getValues(mydata$areas, variable="LIGNITE")
#' getValues(mydata$clusters, variable = "NODU")
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
