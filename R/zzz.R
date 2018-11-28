#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import antaresRead
#' @importFrom methods is
#' @importFrom stats median quantile sd as.formula

.onLoad <- function(libname, pkgname){
  setAlias(
    "downwardMargin",
    "Data required by 'addDownwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "pumpingCapacity",
      "ROW BAL.", "linkCapacity", "mustRun")
  )

  setAlias(
    "upwardMargin",
    "Data required by 'addUpwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "pumpingCapacity", "storageCapacity",
      "ROW BAL.", "AVL DTG", "linkCapacity", "hydroStorageMaxPower")
  )

  setAlias(
    "exportsImports",
    "Data required by 'addExportAndImport()'",
    c("areas", "links", "FLOW LIN.", "linkCapacity")
  )

  setAlias(
    "loadFactorLink",
    "Data required by 'addLoadFactorLink()'",
    c("links", "FLOW LIN.", "linkCapacity")
  )

  setAlias(
    "externalDependency",
    "Data required by 'externalDependency()'",
    c("areas", "links", "LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND",
      "SOLAR", "AVL DTG", "FLOW LIN.", "pumpingCapacity", "storageCapacity", "mustRun", "linkCapacity", "hydroStorageMaxPower")
  )

  setAlias(
    "loadFactor",
    "Data required by 'loadFactor()'",
    c("clusters", "thermalModulation", "mcYears", "thermalAvailabilities")
  )

  setAlias(
    "modulation",
    "Data required by 'modulation()'",
    c("areas", "clusters", "mcYears", "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
      "MISC. DTG", "H. STOR", "H. ROR", "SOLAR", "WIND")
  )

  setAlias(
    "netLoadRamp",
    "Data required by function 'netLoadRamp()'",
    c("areas", "mcYears","LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND",
      "SOLAR", "BALANCE", "mustRun")
  )

  setAlias(
    "surplus",
    "Data required by function 'surplus()'",
    c("areas", "links", "mcYears", "LOAD", "MRG. PRICE", "OP. COST",
      "CONG. FEE (ALG.)", "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
      "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "PSP",
      "ROW BAL.", "HURDLE COST")
  )

  setAlias(
    "surplusClusters",
    "Data required by 'surplusClusters()'",
    c("areas", "clusters", "mcYears", "MRG. PRICE")
  )

  setAlias(
    "surplusSectors",
    "Data required by function 'surplusSectors()'",
    c("areas", "clusters", "mcYears", "WIND", "SOLAR", "H. ROR", "H. STOR",
      "MRG. PRICE")
  )

  setAlias(
    "correctBalance",
    "Data required by function 'correctBalance()'",
    c("areas", "BALANCE", "ROW BAL.")
  )
  sapply(names(pkgEnv$process), function(X){
    tpAlias <- pkgEnv$process[[X]]
    X <- paste0("Out_", X)
    sapply(names(tpAlias), function(Y){
      varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
    })
  })
}

globalVariables(
  c(
    ".", "AVL DTG", "BALANCE", "CONG. FEE (ALG.)", "FLOW LIN.", "H. ROR", "LOAD",
    "MISC. NDG", "MRG. PRICE", "NODU", "OV. COST", "PSP", "ROW BAL.", "SOLAR",
    "WIND", "absoluteModulation", "area", "areaRamp", "availableUnits",
    "balanceRamp", "cluster", "congestionFees", "consumerSurplus", "direction",
    "district", "downwardModulation", "economicGradient", "exportsLevel",
    "fixed.cost", "fixedCost", "from", "globalSurplus", "group", "hstorPMaxAvg",
    "importsLevel", "isolatedDownwardMargin", "isolatedUpwardMargin", "link",
    "marginal.cost", "max_absoluteModulation", "max_downwardModulation",
    "max_upwardModulation", "mcYear", "min.stable.power",
    "minGenModulation", "mustRunTotal", "netLoad", "nominalcapacity", "opCost",
    "prodLastUnit", "producerSurplus", "production", "pumpingCapacity",
    "quantile", "rowBalanceSurplus", "shiftProd", "spinning",
    "startup.cost", "startupCost", "storageCapacity", "storageSurplus",
    "surplusPerUnit", "thermalPmin", "timeId", "to", "toDistrict",
    "totalSurplus", "transCapacityDirect", "transCapacityIndirect",
    "unitcount", "upwardModulation", "variableCost",
    "interconnectedDownwardMargin", "interconnectedUpwardMargin",
    "thermalAvailability", "OP. COST"
  )
)

.idCols <- antaresRead:::.idCols
.addClassAndAttributes <- antaresRead:::.addClassAndAttributes
.groupByDistrict <- antaresRead:::.groupByDistrict
#TODO add after antaresRead 2.2.2
#.check_x <- antaresRead:::.check_x
#.h5Antares_edit_variable <- antaresRead:::.h5Antares_edit_variable
#.skipFunctionH5 <- antaresRead:::.skipFunctionH5
#.check_if_h5_is_in_tmp <- antaresRead:::.check_if_h5_is_in_tmp
#.isSimOpts <- antaresRead:::.isSimOpts
.get_by_area <- antaresRead:::.get_by_area
.get_by_link <- antaresRead:::.get_by_link

pkgEnv <- antaresRead:::pkgEnv

#-----------------------------  HDF5 ------------------------------------#

.requireRhdf5_Antares <- antaresRead:::.requireRhdf5_Antares
# Process H5

pkgEnv$process$addNetLoad$areas <- c("netLoad")
pkgEnv$process$addNetLoad$districts <- c("netLoad")

pkgEnv$process$correctBalance$areas <- c("oldBalance")
pkgEnv$process$correctBalance$districts <- c("oldBalance")

pkgEnv$process$addDownwardMargin$areas <- c("isolatedDownwardMargin",
                                                      "interconnectedDownwardMargin")

pkgEnv$process$addDownwardMargin$districts <- c("isolatedDownwardMargin",
                                            "interconnectedDownwardMargin")

pkgEnv$process$addUpwardMargin$areas <- c("isolatedUpwardMargin",
                                                    "interconnectedUpwardMargin")
pkgEnv$process$addUpwardMargin$districts <- c("isolatedUpwardMargin",
                                          "interconnectedUpwardMargin")

pkgEnv$process$addExportAndImport$areas <- c("import",
                                              "export",
                                             "capExport",
                                             "capImport")
pkgEnv$process$addExportAndImport$districts <- c("import",
                                             "export",
                                             "capExport",
                                             "capImport")

pkgEnv$process$addLoadFactorLink$links <- c("loadFactor",
                                                      "congestion")

pkgEnv$process$externalDependency$areas <- c("netLoad",
                                                       "exportsLevel",
                                                       "importsLevel",
                                                       "exportsFrequency",
                                                       "importsFrequency")

pkgEnv$process$externalDependency$districts <- c("netLoad",
                                             "exportsLevel",
                                             "importsLevel",
                                             "exportsFrequency",
                                             "importsFrequency")



pkgEnv$process$loadFactor$clusters <- c("loadFactor", "propHoursMinGen", "propHoursMaxGen", "loadFactorAvailable")

pkgEnv$process$modulation$clusters <- c("upwardModulation", "downwardModulation",
                                                  "absoluteModulation")

pkgEnv$process$netLoadRamp$areas <- c("netLoadRamp", "balanceRamp", "areaRamp")
pkgEnv$process$netLoadRamp$districts <- c("netLoadRamp", "balanceRamp", "areaRamp")

pkgEnv$process$surplus$areas <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                            "storageSurplus", "congestionFees", "globalSurplus")

pkgEnv$process$surplus$districts <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                  "storageSurplus", "congestionFees", "globalSurplus")


pkgEnv$process$surplusClusters$clusters <- c("variableCost", "fixedCost", "startupCost",
                                                       "surplusPerUnit", "totalSurplus", "economicGradient", "surplusLastUnit")





#pkgEnv$process$surplusSectors$areas <- c("surplus", "cost")

pkgEnv$processDispo <- data.frame(
  trtName = c(
              "netLoad",
              "downwardMargin",
              "upwardMargin",
              "exportsImports",
              "loadFactorLink",
              "externalDependency",
              "loadFactor",
              "modulation",
              "netLoadRamp",
              "surplus",
              "surplusClusters"
  )
  , fctname = c(
                 "addNetLoad",
                 "addDownwardMargin" ,
                 "addUpwardMargin" ,
                 "addExportAndImport" ,
                 "addLoadFactorLink" ,
                 "externalDependency" ,
                 "loadFactor" ,
                 "modulation" ,
                 "netLoadRamp" ,
                 "surplus" ,
                 "surplusClusters"
  ))



.check_if_h5_is_in_tmp<-function(h5filePath=NULL, path=NULL, stop=FALSE, printMessage=TRUE){

  resH5NotInTmp<-!grepl("Temp", h5filePath, ignore.case = TRUE) & !grepl("tmp", h5filePath, ignore.case = TRUE)
  if(resH5NotInTmp){
    if(printMessage){
      print(paste0("h5file : ", h5filePath))
      print(paste0("path : ", path))
    }
  }else{
    return(TRUE)
  }

  messageToPrint<-"h5file is not in temp folder"
  if(stop & resH5NotInTmp){
    stop(messageToPrint)
  }
  if(resH5NotInTmp){
    if(printMessage){
      message(messageToPrint)
    }
  }

  return(FALSE)
}

.skipFunctionH5<-function(h5file = NULL){
  testthat::skip_if(is.null(h5file), "h5file is null")
  if(!is.null(h5file)){
    testthat::skip_if(!.check_if_h5_is_in_tmp(h5file), "h5file is not in temp folder")
  }
  testthat::skip_if_not(.requireRhdf5_Antares(stopP = FALSE),
                        "please install a correct rhdf5 version")

  #I dont why but according to CRAN Team antaresProcessing try to write in the user library
  # but there are checks to verify that h5 file is in tmp folder
  #to comment in the futur
  testthat::skip_on_cran()
  #TODO DEL
  testthat::skip("h5")
}

.cleanDataCluster <- function(clDes = NULL){
  for(columnC in names(clDes)){
    if(class(clDes[[columnC]]) %in% c("double", "numeric", "integer")){
      clDes[is.na(get(columnC)), (columnC) := 0]
    }
  }
  nameMustRun <- "must.run"
  if(nameMustRun %in% names(clDes)){
    clDes[is.na(get("must.run")), ("must.run") := FALSE]
  }else{
    clDes[, (nameMustRun) := FALSE]
  }
  return(clDes)
}

#TODO DEL after antaresRead 2.2.2
#' Test antaresData
#'
#' @param x if x is antaresData class
#'
#' @noRd
.isAntaresData <- function(x){
  "antaresData" %in% class(x)
}

#TODO DEL after antaresRead 2.2.2
.check_x <- function(x = NULL){
  if ("list" %in% class(x)){
    for (elementI in x){
      .check_x(elementI)
    }
  }else{
    if (!(.isSimOpts(x) | .isAntaresData(x))){
      stop(paste0(substitute(x),
                  " should be an object of class 'antaresData' (or 'simOptions') created with 'readAntares()' (or 'setSimulationPath()')"))
    }else{
      return(TRUE)
    }
  }
}

#TODO DEL after antaresRead 2.2.2
#' edit h5 file for TEST
#' currently for all data except clusters
#'
#' @param pathH5 path H5 file
#' @param instanceData character name of one instance
#' @param classData character name of class instance
#' @param timeId timeId to change
#' @param antVar antares Variable to change
#' @param newValue the newValue
#'
#' @noRd
.h5Antares_edit_variable <- function(pathH5 = NULL,
                                     instanceData = NULL,
                                     classData = NULL,
                                     timeId = 1,
                                     antVar = NULL,
                                     newValue = NULL,
                                     mcYear = NULL,
                                     allTimeId = FALSE,
                                     timeStep = "hourly"){

  if (is.null(instanceData) | is.null(classData)){
    stop("instanceData and classData must be set together")
  }

  if (classData=="areas"){
    classDataS <- "area"
  }else if(classData=="links"){
    classDataS <- "link"
  }else{
    classDataS <- "district"
  }

  if (is.null(mcYear)){
    typeOfData <- "/mcAll"
  }else{
    typeOfData <- "/mcInd"
  }
  timeStepType <- paste0("/",paste(timeStep, classData, sep = "/"))
  typeOfDataTime <- paste0(timeStepType, typeOfData)
  nameStructure <- paste0(typeOfDataTime, "/structure")

  H5locAntaresh5 <- rhdf5::H5Fopen(name = pathH5)
  resStruc <- rhdf5::h5ls(H5locAntaresh5)
  dimData <- resStruc[ resStruc$group == typeOfDataTime & resStruc$name == "data", ]$dim
  hourlyDataStructure <- rhdf5::h5read(H5locAntaresh5, name = nameStructure)

  indexCateroryInstance <- grep(instanceData, hourlyDataStructure[[classDataS]])[1]

  indexAntVar <- grep(antVar, hourlyDataStructure$variable)[1]
  if(is.na(indexAntVar)){
    indexAntVar <- grep(antVar, hourlyDataStructure$reCalcVar)[1]
    if(is.na(indexAntVar)){
      stop("error index")
    }else{
      indexAntVar <- indexAntVar + length(hourlyDataStructure$variable)
    }
  }
  if(allTimeId){
    maxTimeId <- as.integer(strsplit(dimData, "x")[[1]][1])
    indexTimeId <- 1:maxTimeId
  }else{
    indexTimeId <- timeId
  }

  if (is.null(mcYear)){
    indexMcYear <- 1
  }else{
    indexMcYear <- grep(mcYear, hourlyDataStructure$mcYear)[1]
  }

  listIndex <- list(indexTimeId, indexAntVar, indexCateroryInstance, indexMcYear)
  #debug print(listIndex)

  hourlyData <- rhdf5::h5read(
    H5locAntaresh5,
    name = paste0(typeOfDataTime, "/data"),
    index = listIndex)

  hourlyData[,,,] <- newValue

  rhdf5::h5writeDataset.array(
    obj = hourlyData,
    h5loc = H5locAntaresh5,
    name = paste0(typeOfDataTime, "/data"),
    index = listIndex
  )

  rhdf5::H5Fclose(h5file = H5locAntaresh5)
  rhdf5::h5closeAll()
}


#' Test opst
#'
#' @param test if x is simOptions class
#'
#' @noRd
.isSimOpts <- function(x){
  if ("simOptions" %in% class(x)){
    if (!is.null(x$h5path)){
      if (!file.exists(x$h5path)){
        warning(paste0("h5file does not exists for this study :",
                       x$studyName))
        return(FALSE)
      }else{
        return(TRUE)
      }
    }else{
      #opts but no h5 (TXT)
      return(TRUE)
    }
  }else{
    return(FALSE)
  }
}
