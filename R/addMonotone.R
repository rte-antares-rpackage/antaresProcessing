#Copyright © 2016 RTE Réseau de transport d’électricité

#' addMonotones
#'
#' This function compute monotone for some variables.
#'
#' @return
#' \code{addMonotones} modifies its input by adding monotones.
#' @inheritParams addUpwardMargin
#'
#' @examples
#' \dontrun{
#' # First simulation
#' studyPath <- "path/to/study/"
#'
#' setSimulationPath(studyPath, 1)
#' mydata1 <- readAntares(areas = "all", districts = "all", synthesis = FALSE)
#' addMonotones(mydata1)
#'
#' }
#'
#' @export
#'
addMonotones <- function(antaresData = NULL){

  if(attr(antaresData, "synthesis")){
    stop("antaresData are synthesis, addMonotones() needs detail data.")
  }

  if(is(antaresData, "antaresDataList")){
    for(na in names(antaresData)){
      addMonotones(antaresData[[na]])
    }
  }else{
    if(length(unique(antaresData$mcYear)) <= 1){
      stop("antaresData must contain at least two mcYears.")
    }
    #variable to take from x
    idCols <- .idCols(antaresData)
    varToTake <- setdiff(names(antaresData), idCols)
    antaresDataCopy <- copy(antaresData[, mget(c(idCols, varToTake))])
    #by of x
    getByA <- .get_by_area((antaresDataCopy))
    getByA <- setdiff(getByA, "timeId")
    #get only what you want
    nameNewColum <- paste0(varToTake, "Mono")
    antaresDataCopy[, eval(nameNewColum) := 0L]
    #maybe faster but we must deal with column/string
    # resMono <- antaresData[, eval(quote(.(nameNewColum =
    #                                       sort(get(varToTake),
    #                                            decreasing = TRUE)))),
    #                        by = getByA]
    antaresDataCopy <- antaresDataCopy[, (nameNewColum) := sort(get(varToTake),
                                                   decreasing = TRUE),
                by = getByA]
    antaresDataCopy[, c(varToTake) := NULL]
    #Cast the result to be able to make a rowMean
    IdColsWMcYear <- idCols[idCols!="mcYear"]
    myFormula <- sprintf("%s ~ mcYear", paste(IdColsWMcYear, collapse = "+"))
    antaresDataCopy <- data.table::dcast(data = antaresDataCopy,
                                 formula = as.formula(myFormula),
                                 value.var = c(paste0(varToTake, "Mono")))
    data.table::setnames(x = antaresDataCopy,
                         old = as.character(unique(antaresData$mcYear)),
                         new = paste0(varToTake,"MonoMc" , unique(antaresData$mcYear)))
    #make a monotone rowMean
    nameNewColum <- c(paste0(varToTake, "MonoMean"))
    antaresDataCopy[, eval(nameNewColum) := 0L]
    antaresDataCopy <- antaresDataCopy[, eval(nameNewColum) := as.integer(round(rowMeans(.SD), 0)),
                           by = IdColsWMcYear, .SDcols = c("LOADMonoMc1", "LOADMonoMc2")]
    #antaresData <- merge(antaresData, resMonoMean, by = IdColsWMcYear)
    setkeyv(antaresDataCopy, IdColsWMcYear)
    antaresData <- merge(antaresData, resMonoMean, by = IdColsWMcYear)
    setcolorder(antaresData, c(idCols, varToTake, nameNewColum))
    invisible(antaresData)
  }
}


