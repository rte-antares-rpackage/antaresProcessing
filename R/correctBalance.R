#Copyright © 2016 RTE Réseau de transport d’électricité

#' correctBalance
#'
#' This function correct the BALANCE with 'ROW BAL.'.
#'
#' @return
#' \code{correctBalance} modifies its input by editing BALANCE and 'ROW BAL.'.
#' Formulas :
#' \enumerate{
#' \item \eqn{BALANCE = BALANCE - 'ROW. BAL.'}
#' \item \eqn{ROW. BAL = 0}
#' }
#'
#' @inheritParams addUpwardMargin
#'
#' @examples
#' \dontrun{
#' # First simulation
#' studyPath <- "path/to/study/"
#'
#' setSimulationPath(studyPath, 1)
#' mydata1 <- readAntares(areas = "all", districts = "all", synthesis = FALSE)
#' correctBalance(mydata1)
#'
#' }
#'
#' @export
#'
correctBalance <- function(x) {
  .check_x(x)

  if(.isSimOpts(x)){
    .correctBalanceOpts(x)
    return(invisible())
  }else{
    if (is(x, "antaresDataList")) {
      if(!is.null(x$areas)){
        x$areas <- .correctBalanceAreas(x$areas)
      }
      if(!is.null(x$districts)){
        x$districts <- .correctBalanceAreas(x$districts)
      }
    }else{
      if("area" %in% names(x) | "district" %in% names(x)){
        x <- .correctBalanceAreas(x)
      }
    }
  }

  return(x)

}

.correctBalanceAreas <- function(x){
  .check_x(x)
  oldBalance <- NULL
  x[, oldBalance := BALANCE]
  x[, BALANCE := BALANCE - `ROW BAL.`]
  x[, `ROW BAL.` := 0]

  return(x)

}

.correctBalanceOpts <- function(optsP = NULL){

  sapply(pkgEnv$timeStepPosible, function(timeStepS){
    for(mcYearIns in c("mcInd", "mcAll")){
      suppressWarnings(addProcessingH5(opts = optsP,
                      mcY = mcYearIns,
                      timeStep = timeStepS,
                      evalAreas = list(
                        oldBalance = "BALANCE"
                      ),
                      evalDistricts = list(
                        oldBalance = "BALANCE"
                      )))
      #read the data
      if(mcYearIns=="mcInd"){
        mcYearRead <- "all"
      }else{
        mcYearRead <- NULL
      }
      dataRead <- readAntares(opts = optsP,
                              areas = "all",
                              districts = "all",
                              select = c("BALANCE", "ROW BAL."),
                              mcYears = mcYearRead,
                              timeStep = timeStepS)
      #correct the data
      dataCor <- correctBalance(x = dataRead)
      #write the corrected data
      .writeH5Antares(dataC = dataCor, opts = optsP, timeStepC = timeStepS)
    }
  })
}

.writeH5Antares <- function(dataC = NULL, opts = NULL, timeStepC = NULL){
  #loop data
  for(typeData in c("areas", "districts")){
    if(typeData=="district"){
      print("test")
    }
    dataToC <- dataC[[typeData]]
    ifelse(typeData=="areas", keyData <- "area", keyData <- "district")
    if(!is.null(dataToC)){
      keyToW <- unique(dataToC[[keyData]])
      #correct only the values
      varToCorrect <- setdiff(names(dataToC), .idCols(dataToC))
      if(attr(dataC, "synthesis")){
        sapply(varToCorrect, function(varS){
          sapply(keyToW, function(keyToS){
            timeIdW <- unique(dataToC[get(keyData)==keyToS, timeId])
            dataCVar <- dataToC[get(keyData)==keyToS, get(varS)]
            .h5Antares_edit_variable(
              pathH5 = opts$h5path,
              instanceData = keyToS,
              classData = typeData,
              timeId = timeIdW,
              antVar = varS,
              newValue = dataCVar,
              mcYear = NULL,
              allTimeId = TRUE,
              timeStep = timeStepC
            )
          })
        })
      }else{
        mcYearW <- unique(dataToC$mcYear)
        sapply(X = mcYearW, FUN = function(mcYearS){
          sapply(varToCorrect, function(varS){
            sapply(keyToW, function(keyToS){
              timeIdW <- unique(dataToC[get(keyData)==keyToS, timeId])
              dataCVar <- dataToC[get(keyData)==keyToS & mcYear == mcYearS,
                                  get(varS)]
              .h5Antares_edit_variable(
                pathH5 = opts$h5path,
                instanceData = keyToS,
                classData = typeData,
                timeId = timeIdW,
                antVar = varS,
                newValue = dataCVar,
                mcYear = mcYearS,
                allTimeId = TRUE,
                timeStep = timeStepC
              )
            })
          })
        })
      }
    }
  }
}
