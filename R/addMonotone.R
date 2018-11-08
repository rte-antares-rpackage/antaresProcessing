#Copyright © 2016 RTE Réseau de transport d’électricité

#' addMonotones
#'
#' This function compute monotone for some variables.
#' @param antaresData Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}.
#' @param variable An ANTARES variable.
#'
#' @return
#' \code{addMonotones} modifies its input by adding monotones.
#'
#'
#' @examples
#' \dontrun{
#' # First simulation
#' studyPath <- "path/to/study/"
#'
#' setSimulationPath(studyPath, 1)
#' myData1 <- readAntares(areas = "all",
#' districts = "all", synthesis = FALSE)
#' addMonotones(antaresData = myData1,
#' variable = "LOAD")
#'
#' }
#'
#' @export
#'
addMonotones <- function(antaresData = NULL, variable = NULL){

  .check_x(antaresData)

  if(attr(antaresData, "synthesis")){
    stop("antaresData are synthesis, addMonotones() needs detail data.")
  }

  if(!is.character(variable)){
    stop("variable is not a character")
  }

  if(is(antaresData, "antaresDataList")){
    #chack if variable is correct
    nameCol <- NULL
    for(i in 1:length(antaresData)){
      nameCol <- c(nameCol, names(antaresData[[i]]))
    }
    if(!(variable %in% nameCol)){
      stop("incorrect variable")
    }
    #call antaresData for an antaresDataTable
    for(na in names(antaresData)){
      if(variable %in% names(antaresData[[na]])){
        addMonotones(antaresData[[na]], variable = variable)
      }
    }
  }else{

    if(!(variable %in% names(antaresData))){
      stop("Incorrect variable")
    }
    classVariable <- class(antaresData[[variable]])
    if(classVariable %in% c("factor", "POSIXt", "character")){
      stop("Incorrect variable class")
    }else if(classVariable=="numeric"){
      funcClass <- as.numeric
      digitMonoMean <- 2
    }else if(classVariable=="integer"){
      funcClass <- as.integer
      digitMonoMean <- 0
    }else if(classVariable=="double"){
      funcClass <- as.double
      digitMonoMean <- 2
    }else{
      stop("wrong class variable")
    }

    if(length(unique(antaresData$mcYear)) <= 1){
      stop("antaresData must contain at least two mcYears.")
    }
    #variable to take from x
    idCols <- .idCols(antaresData)
    varToTake <- variable


    antaresDataCopy <- copy(antaresData[, mget(c(idCols, varToTake))])
    #by of x
    getByA <- setdiff(getIdCols(antaresDataCopy), pkgEnv$idTimeVars)

    #get only what you want
    nameNewColumn <- paste0(varToTake, "Mono")
    antaresDataCopy[, (nameNewColumn) := funcClass(0)]
    #maybe faster but we must deal with column/string
    # resMono <- antaresData[, eval(quote(.(nameNewColumn =
    #                                       sort(get(varToTake),
    #                                            decreasing = TRUE)))),
    #                        by = getByA]
    antaresDataCopy <- antaresDataCopy[, (nameNewColumn) := sort(get(varToTake),
                                                   decreasing = TRUE),
                by = getByA]
    antaresDataCopy[, c(varToTake) := NULL]
    #Cast the result to be able to make a rowMean
    IdColsWMcYear <- idCols[idCols!="mcYear"]
    myFormula <- sprintf("%s ~ mcYear", paste(IdColsWMcYear, collapse = "+"))
    antaresDataCopy <- data.table::dcast(data = antaresDataCopy,
                                 formula = as.formula(myFormula),
                                 value.var = c(paste0(varToTake, "Mono")))
    newNameColumnnMonoMc <- paste0(varToTake,"MonoMc" , unique(antaresData$mcYear))
    data.table::setnames(x = antaresDataCopy,
                         old = as.character(unique(antaresData$mcYear)),
                         new = newNameColumnnMonoMc)
    #make a monotone rowMean
    nameNewColum <- c(paste0(varToTake, "MonoMean"))
    antaresDataCopy[, (nameNewColum) := funcClass(0)]
    antaresDataCopy <- antaresDataCopy[, (nameNewColum) := funcClass(round(rowMeans(.SD), digitMonoMean)),
                           by = IdColsWMcYear, .SDcols = newNameColumnnMonoMc]
    #antaresData <- merge(antaresData, resMonoMean, by = IdColsWMcYear)
    colToAdd <- setdiff(names(antaresDataCopy), IdColsWMcYear)
    antaresData[antaresDataCopy,
                (colToAdd) := mget(paste0("i.", colToAdd)),
                on = IdColsWMcYear]
    invisible(antaresData)
  }
}


