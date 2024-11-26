#Copyright © 2016 RTE Réseau de transport d’électricité

#' Merge all antaresDataSets
#'
#' @param dta antaresData
#'
#'@return A data.table, data.frame class object containing the study's
#'output data (wide format)
#'
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
#' mydata <- readAntares( areas = "all",
#'                        mcYears = "all", showProgress = FALSE)
#'
#' # long to wide format
#' mydata <- mergeAllAntaresData(mydata)
#' }
#'
#' @export
mergeAllAntaresData <- function(dta)
{
  commonId <- .idCols(dta)
  if("antaresDataTable" %in%class(dta)){
    .dcastAntaresDataTable(dta)
  }else{
    communId <- .idCols(dta[[1]])
    ctR <- .idCols(dta[[1]], removeTimeId = TRUE)
    ctR <- ctR[ctR != "mcYear"]
    communId <- communId[!communId%in%ctR]
    mymerge = function(x,y) merge(x,y,by = communId)
    Reduce(mymerge,lapply(dta, .dcastAntaresDataTable))
  }
}

.dcastAntaresDataTable <- function(data){
  idC <- .idCols(data)
  idNd <- .idCols(data, removeTimeId = TRUE)
  idNd <- idNd[idNd != "mcYear"]
  useCol <- idC
  OthrCOl <- names(data)[!names(data)%in%idC]
  idC <- idC[!idC %in% idNd]
  idC <- paste(idC, collapse = "+")
  idNd <- paste(idNd, collapse = "+")
  R <- dcast(data,paste(idC, "~",idNd), value.var = OthrCOl)
  R
}
