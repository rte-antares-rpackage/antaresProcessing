#Copyright © 2016 RTE Réseau de transport d’électricité

#' Merge all antaresDataSets
#'
#' @param dta antaresData
#'
#' @examples
#' \dontrun{
#' setSimulationPath("Mystud", 1)
#' dta <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all")
#' dta <- mergeAllAntaresData(dta)
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
