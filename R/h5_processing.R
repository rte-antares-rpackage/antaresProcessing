#' Add process results of antaresProcessing to an ANTARES .h5 files
#'
#' @description In this version only hourly data can be enriched.
#'
#' @param opts \code{simOptions} obtain which \link[antaresRead]{setSimulationPath}
#' @param mcY  \code{character}, "mcInd" or "mcAll".
#' @param timeStep \code{character}, timeStep
#' @param addNetLoad  \code{boolean} refer to \link[antaresProcessing]{addNetLoad}
#' @param addDownwardMargin \code{boolean} refer to \link[antaresProcessing]{addDownwardMargin}
#' @param addUpwardMargin \code{boolean} refer to \link[antaresProcessing]{addUpwardMargin}
#' @param addExportAndImport \code{boolean} refer to \link[antaresProcessing]{addExportAndImport}
#' @param addLoadFactorLink \code{boolean} refer to \link[antaresProcessing]{addLoadFactorLink}
#' @param externalDependency \code{boolean} refer to \link[antaresProcessing]{externalDependency}
#' @param loadFactor \code{boolean} refer to \link[antaresProcessing]{loadFactor}
#' @param modulation \code{boolean} refer to \link[antaresProcessing]{modulation}
#' @param netLoadRamp \code{boolean} refer to \link[antaresProcessing]{netLoadRamp}
#' @param surplus \code{boolean} refer to \link[antaresProcessing]{surplus}
#' @param surplusClusters \code{boolean} refer to \link[antaresProcessing]{surplusClusters}
#' @param thermalAvailabilities \code{boolean} Should the surplus of the last unit of a cluster be computed by \link[antaresProcessing]{surplusClusters}.
#' Should loadFactorAvailable be added to the result of \link[antaresProcessing]{loadFactor}.
#' @param allProcess \code{boolean} All process in one argument.
#' @param linkCapacity \code{boolean} should export and import capacities be computed by \link[antaresProcessing]{addExportAndImport}.
#' @param mustRun \code{boolean} should the production in must run mode subtracted to the net load \link[antaresProcessing]{addNetLoad}.
#' Should the must run production be ignored in the computation of the netLoadRamp see \link[antaresProcessing]{netLoadRamp}.
#' @param evalAreas \code{list}, list of operation to evaluate in areas data
#' @param evalLinks \code{list}, list of operation to evaluate in links data
#' @param evalClusters \code{list}, list of operation to evaluate in clusters data
#' @param evalDistricts \code{list}, list of operation to evaluate in districts data
#' @param nThreads \code{numeric}, nThreads to use
#'
#' @details
#'
#' When you add a straitment, an alias is created. They can be used for request h5 file. See examples.
#'
#' Available alias are :
#' \itemize{
#'    \item "Out_addDownwardMargin"
#'    \item "Out_addUpwardMargin"
#'    \item "Out_addExportAndImport"
#'    \item "Out_addLoadFactorLink"
#'    \item "Out_externalDependency"
#'    \item "Out_loadFactor"
#'    \item "Out_modulation"
#'    \item "Out_netLoadRamp"
#'    \item "Out_surplus"
#'    \item "Out_surplusClusters"
#'    }
#'
#' @examples
#'
#' \dontrun{
#' addProcessingH5(opts = opts,  mcY = "mcInd",
#'                addDownwardMargin = TRUE,
#'                addUpwardMargin = TRUE,
#'                addExportAndImport = TRUE,
#'                addLoadFactorLink = TRUE,
#'                externalDependency = TRUE,
#'                loadFactor = TRUE,
#'                modulation = TRUE,
#'                netLoadRamp = TRUE,
#'                surplus = TRUE,
#'                surplusClusters = TRUE,
#'                evalAreas = list(Tota = "`H. STOR` + `MISC. DTG`",
#'                                 Tota2 = "`NODU` + `NP COST` + 1"),
#'                evalLinks = list(),
#'                evalClusters = list(),
#'                evalDistricts = list()
#'                )
#'
#' #After write of new columns, new aliases are avialable in antaresRead.You can use
#' #showAliases() to see them. Prifix Out_ is used to distinguish them.
#' showAliases("Out_surplusClusters")
#' readAntares(opts = opts, select = "Out_surplusClusters")
#'
#' }
#'
#' @export
addProcessingH5 <- function(opts = simOptions(),
                            mcY = c("mcInd", "mcAll"),
                            timeStep = "hourly",
                            addNetLoad = FALSE,
                            addDownwardMargin = FALSE,
                            addUpwardMargin = FALSE,
                            addExportAndImport = FALSE,
                            addLoadFactorLink = FALSE,
                            externalDependency = FALSE,
                            loadFactor = FALSE,
                            modulation = FALSE,
                            netLoadRamp = FALSE,
                            surplus = FALSE,
                            surplusClusters = FALSE,
                            thermalAvailabilities = FALSE,
                            linkCapacity = FALSE,
                            mustRun = FALSE,
                            allProcess = FALSE,
                            evalAreas = list(),
                            evalLinks = list(),
                            evalClusters = list(),
                            evalDistricts = list(), nThreads = 1){

  .setAliasH5()

  if(!isH5Opts(opts)){
    stop("opts not refear to an h5 file")
  }

  allData <- allProcess
  if(allData){
    addNetLoad <- TRUE
    addDownwardMargin <- TRUE
    addUpwardMargin <- TRUE
    addExportAndImport <- TRUE
    addLoadFactorLink <- TRUE
    externalDependency <- TRUE
    loadFactor <- TRUE
    modulation <- TRUE
    netLoadRamp <- TRUE
    surplus <- TRUE
    surplusClusters <- TRUE
  }

  if(netLoadRamp){
    externalDependency <- TRUE
  }



  mcY <- match.arg(mcY)

  if(mcY == "mcAll"){
    nThreads = 1
  }
  allStraitments <- list(
    addNetLoad = addNetLoad,
    addDownwardMargin = addDownwardMargin,
    addUpwardMargin = addUpwardMargin,
    addExportAndImport = addExportAndImport,
    addLoadFactorLink = addLoadFactorLink,
    externalDependency = externalDependency,
    loadFactor = loadFactor,
    modulation = modulation,
    netLoadRamp = netLoadRamp,
    surplus = surplus,
    surplusClusters = surplusClusters)

  columnsToAdd <- .getNewColumnsName(allStraitments)
  writeAreas <- ifelse(is.null(columnsToAdd$areas) & length(evalAreas) == 0, FALSE, TRUE)
  writeLinks <- ifelse(is.null(columnsToAdd$links) & length(evalLinks) == 0, FALSE, TRUE)
  writeClusters <- ifelse(is.null(columnsToAdd$clusters) & length(evalClusters) == 0, FALSE, TRUE)
  writeDistricts <- ifelse(is.null(columnsToAdd$districts) & length(evalDistricts) == 0, FALSE, TRUE)

  select <- .getSelectAlias(allStraitments)


  ##Load removeVirtAreas
  if(allStraitments$surplus == TRUE)
  {
    .requireRhdf5_Antares(stopP = TRUE)
    fid <- rhdf5::H5Fopen(opts$h5path)
    removeAre <- .loadAttributes(fid, "hourly")
    rhdf5::H5Fclose(fid)
    if("virtualNodes" %in% names(removeAre))
    {
      removeAre <- unlist(removeAre$virtualNodes)
      select <- c(select, removeAre)
    }

  }

  columnsToSelects <- unique(unlist(lapply(list(evalAreas,evalLinks,evalClusters,  evalDistricts ), function(Z){
    lapply(Z, function(X){
      strsplit(X, "`")
    })
  })))

  ##Load first Mcyear

  if(mcY == "mcInd")
  {
    mcYear <- opts$mcYears
    # if(!isTRUE(all.equal(1:length(mcYear), mcYear))){
    #   mcYear <- 1:length(mcYear)
    # }
  }
  if(mcY == "mcAll")
  {
    mcYear <- "mcAll"
  }
  # timeStep <- "hourly"


  if(mcYear[1] != 'mcAll')
  {
    by = nThreads
    mcYear_L <- vector("list", floor((length(mcYear)-1)/by) + 1 )
    for(i in 1:length(mcYear))
    {
      mcYear_L[[floor((i-1)/by) + 1]] <- c(mcYear_L[[floor((i-1)/by) + 1]], mcYear[i])
    }
    mcYear <- mcYear_L
  }else{
    mcYear <- list(mcYear)
  }
  if(nThreads > 1){

    if(!requireNamespace("parallel")) stop("Error loading 'parallel' package.")

    cl <- parallel::makeCluster(length(mcYear[[1]]))
    Parallel = TRUE

    parallel::clusterExport(cl, c("opts"), envir = environment())
    parallel::clusterEvalQ(cl = cl, {
      require(antaresProcessing)
      .setAliasH5()
      opts <- setSimulationPath(opts$h5path)
    })

  }else{
    Parallel = FALSE
  }

  outToWrite <- lapply(1:length(mcYear), function(x){
    X <- mcYear[[x]]
    if(X[1] == "mcAll"){
      X <- NULL
    }

    if(!Parallel)
    {
      myOut <- .readDataEndAddColumn(opts, select = select, mcYears = X, timeStep = timeStep,
                                     evalAreas = evalAreas, evalLinks = evalLinks,
                                     evalClusters = evalClusters, evalDistricts = evalDistricts,
                                     columnsToSelects = columnsToSelects, allStraitments = allStraitments,
                                     writeAreas = writeAreas,
                                     writeLinks = writeLinks,
                                     writeClusters = writeClusters,
                                     writeDistricts = writeDistricts,
                                     columnsToAdd = columnsToAdd,
                                     linkCapacity = linkCapacity, mustRun = mustRun,
                                     thermalAvailabilities = thermalAvailabilities)
    }else{
      parallel::clusterExport(cl, c("opts", "select", "X",  "timeStep",
                                    "evalAreas", "evalLinks",
                                    "evalClusters", "evalDistricts",
                                    "columnsToSelects","allStraitments",
                                    "writeAreas",
                                    "writeLinks",
                                    "writeClusters",
                                    "writeDistricts",
                                    "columnsToAdd",
                                    "linkCapacity",
                                    "mustRun",
                                    "thermalAvailabilities"), envir = environment())
      myOut <- parallel::parSapply(cl, X, function(Y){
        .readDataEndAddColumn(opts, select = select, mcYears = Y, timeStep = timeStep,
                              evalAreas = evalAreas, evalLinks = evalLinks,
                              evalClusters = evalClusters, evalDistricts = evalDistricts,
                              columnsToSelects = columnsToSelects, allStraitments = allStraitments,
                              writeAreas = writeAreas,
                              writeLinks = writeLinks,
                              writeClusters = writeClusters,
                              writeDistricts = writeDistricts,
                              columnsToAdd = columnsToAdd,
                              linkCapacity = linkCapacity, mustRun = mustRun,
                              thermalAvailabilities = thermalAvailabilities)
      }, simplify = FALSE)


      namS <- names(myOut[[1]])
      myOut <- sapply(1:length(myOut[[1]]), function(V){
        rbindlist(sapply(1:length(myOut), function(W){
          myOut[[W]][[V]]
        }, simplify = FALSE))
      }, simplify = FALSE)
      names(myOut) <- namS

    }

    outList <- names(myOut)
    myOut$areas
    outToWrite <- sapply(outList, function(HH){
      tp <- as.matrix(myOut[[HH]])
      tp[is.na(tp)] <- -9999
      tp
    }, simplify = FALSE)


    if(is.null(X)){
      writeStruct <- TRUE
    }else{
      writeStruct <- unique(X == mcYear[[1]])
    }


    writeAreas <- "areas" %in% names(outToWrite)
    writeLinks <- "links" %in% names(outToWrite)
    writeClusters <- "clusters" %in% names(outToWrite)
    writeDistricts <- "districts" %in% names(outToWrite)


    .writeAllTables(timeStep = timeStep,
                    mcY = mcY,
                    path = opts$h5path,
                    outToWrite = outToWrite ,
                    areas = writeAreas,
                    links = writeLinks,
                    clusters = writeClusters,
                    districts = writeDistricts,
                    mcYear = x, writeStruct = writeStruct)



  })
  if(Parallel) parallel::stopCluster(cl)


  ##Add control on straitments to define all these objects

  ##IfverWiteAreas

}

.writeAllTables <- function(timeStep, mcY, path, outToWrite,
                            areas, links, clusters, districts,
                            mcYear = NULL, writeStruct = FALSE){
  .requireRhdf5_Antares(stopP = TRUE)
  fid <- rhdf5::H5Fopen(path)
  sapply(c("areas", "links", "clusters", "districts"), function(X){
    if(get(X)){
      fid <- rhdf5::H5Fopen(path)
      Y <- eval(X)
      GP <- paste0(timeStep, "/", Y, "/", mcY)
      .writeNewColumns(fid = fid,
                       newdata = outToWrite[[Y]],
                       GP = GP, mcYear = mcYear,
                       writeStruct = writeStruct)
    }

  })

}


.getDim <- function(fid, GP, type = "size")
{
  .requireRhdf5_Antares(stopP = TRUE)
  did <- rhdf5::H5Dopen(fid, GP)
  rhdf5::H5Dget_space(did)
  res <- rhdf5::H5Dget_space(did)
  rhdf5::H5Dclose(did)
  dim <- rhdf5::H5Sget_simple_extent_dims(res)[[type]]
  dim
}

.getIndexToWrite <- function(dim, nbVarToWrite, mcYear = NULL){
  d4 <- if(is.null(mcYear)){1:dim[4]}else{mcYear}
  list(1:dim[1], (dim[2] + 1) : (dim[2] + nbVarToWrite), 1:dim[3], d4)
}


.readDataEndAddColumn <- function(opts, select, mcYears, timeStep,
                                  evalAreas, evalLinks,
                                  evalClusters, evalDistricts, columnsToSelects,
                                  allStraitments,
                                  writeAreas,
                                  writeLinks,
                                  writeClusters,
                                  writeDistricts, columnsToAdd,
                                  linkCapacity,
                                  mustRun,
                                  thermalAvailabilities){

  if(writeAreas){
    ar <- "all"
  }else{
    ar <- NULL
  }
  if(writeLinks){
    ln <- "all"
  }else{
    ln <- NULL
  }
  if(writeClusters){
    clu <- "all"
  }else{
    clu <- NULL
  }
  if(writeDistricts){
    dr <- "all"
  }else{
    dr <- NULL
  }

  res <- readAntares(areas = ar,
                     links = ln,
                     clusters = clu,
                     districts = dr,
                     opts = opts, select = c(select,columnsToSelects),
                     mcYears = mcYears, timeStep = timeStep)

  res <- as.antaresDataList(res)
  nrowRes <- lapply(res, nrow)


  # for(i in 1:length(res)){
  #   res[[i]] <- res[[i]][, .SD, .SDcols = names(res[[i]])[!names(res[[i]])%in%select]]
  # }
  res <- .calcNewColumns(res, allStraitments, timeStep = timeStep, linkCapacity = linkCapacity, mustRun = mustRun, thermalAvailabilities = thermalAvailabilities, opts = opts)

  if(writeAreas && "areas" %in% names(res)){
    if(length(evalAreas) > 0)
    {
      res$areas[, names(evalAreas) := lapply(evalAreas, function(X){eval(parse(text = X))})]
    }

    cAdd <- c(columnsToAdd$areas, names(evalAreas))

    res$areas <- res$areas[, .SD, .SDcols = unique(cAdd[cAdd%in%names(res$areas)])]
  }else{
    res$areas <- NULL
  }
  if(writeLinks && "links" %in% names(res)){
    if(length(evalLinks) > 0)
    {
      res$links[, names(evalLinks) := lapply(evalLinks, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$links, names(evalLinks))
    res$links <- res$links[, .SD, .SDcols = unique(cAdd[cAdd%in%names(res$links)])]
  }else{
    res$links <- NULL
  }
  if(writeClusters && "clusters" %in% names(res)){
    if(length(evalClusters) > 0)
    {
      res$clusters[, names(evalClusters) := lapply(evalClusters, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$clusters,names(evalClusters))
    res$clusters <- res$clusters[, .SD, .SDcols =  unique(cAdd[cAdd%in%names(res$clusters)])]
  }else{
    res$clusters <- NULL
  }
  if(writeDistricts && "districts" %in% names(res)){
    if(length(evalDistricts) > 0)
    {
      res$districts[, names(evalDistricts) := lapply(evalDistricts, function(X){eval(parse(text = X))})]
    }
    cAdd <- c(columnsToAdd$districts, names(evalDistricts))
    res$districts <- res$districts[, .SD, .SDcols =  unique(cAdd[cAdd%in%names(res$districts)])]
  }else{
    res$districts <- NULL
  }


  ###Controle data write
  lapply(res, function(X){
    classColumns <- unlist(lapply(X, class))
    wolumnstoTransform <- which(classColumns == "logical")
    if(length(wolumnstoTransform) > 0){
      X[,names(wolumnstoTransform) := lapply(X = .SD, as.numeric), .SDcols = wolumnstoTransform]
      if(mcYears[1] == opts$mcYears[1])
      {
        warning(paste0("Some boolean column(s) found, they will be transform to numeric (TRUE : 1, FALSE : 0)"))
      }
    }
  })

  lapply(res, function(GG){
    if(!all(unlist(lapply(GG, class)) %in% c("numeric", "integer"))){
      concerCol <- names(unlist(lapply(GG, class)))[
        !unlist(lapply(GG, class)) %in% c("numeric", "integer")]
      stop("Somes columns (", paste0(concerCol, collapse = ";") ,") are not numeric, integer or logical they can't be write in h5")
    }
  })

  nrwNrowRes <- lapply(res, nrow)
  for(i in names(nrwNrowRes)){
    if(nrowRes[[i]] != nrwNrowRes[[i]]){
      stop("New file have a diffrent number of row than request file, columns can't be add to h5 file")
    }
  }

  res
}


.writeNewColumns <- function(fid, newdata, GP, mcYear = NULL, writeStruct = FALSE)
{

  .requireRhdf5_Antares(stopP = TRUE)

  nbVarToWrite <- ncol(newdata)
  namesVariable <- colnames(newdata)
  datatype <- paste0(GP, "/data")
  if(writeStruct)
  {
    oldStruct <-  paste0(GP, "/structure/reCalcVar")
    did <- rhdf5::H5Dopen(fid, oldStruct)
    structVarAdd <- rhdf5::H5Dread(did)
    rhdf5::H5Dclose(did)
    if(sum(namesVariable %in% structVarAdd) > 0 )
    {
      warning("Somes columns already exists in h5 file, they will be overwrite.")
      namesVariable <- namesVariable[!namesVariable %in% structVarAdd]

    }
    if(length(namesVariable) > 0){
      structVarAdd[which(structVarAdd == "")][1:length(namesVariable)] <- namesVariable

      #h5write(structVarAdd, path, oldStruct)
      rhdf5::h5writeDataset.array(obj = structVarAdd,  fid, oldStruct)


      # if(grepl("areas", GP))
      # {
      #   attributes <- .loadAttributes(fid, "hourly")
      #   attributes$opts$variables$areas <- unique(c(attributes$opts$variables$areas, namesVariable))
      #   .writeAttributes(path = NULL, timeStep =  "hourly", fid = fid, attributes = attributes)
      #
      # }
      #
      # if(grepl("links", GP))
      # {
      #   attributes <- .loadAttributes(fid, "hourly")
      #   attributes$opts$variables$links <- unique(c(attributes$opts$variables$links, namesVariable))
      #   .writeAttributes(path = NULL, timeStep =  "hourly", fid = fid, attributes = attributes)
      #
      # }

    }
  }

  oldStruct <-  paste0(GP, "/structure/reCalcVar")
  did <- rhdf5::H5Dopen(fid, oldStruct)
  allVarAdd <- rhdf5::H5Dread(did )


  oldStruct <-  paste0(GP, "/structure/variable")
  did <- rhdf5::H5Dopen(fid, oldStruct)
  allnorm <- rhdf5::H5Dread(did )
  allVarAdd <- c(allnorm, allVarAdd)
  indexVar <- sapply(colnames(newdata), function(X){
    which(allVarAdd == X)
  })


  actualDim <- .getDim(fid, datatype)
  indexToWrite <- .getIndexToWrite(actualDim, nbVarToWrite, mcYear)
  dimtowrite <- unlist(lapply(indexToWrite, length))
  indexToWrite[[2]] <- unlist(indexVar)

  arrayToWrite <- array(newdata, dimtowrite[c(1,3,4,2)])
  # dim(arrayToWrite)
  arrayToWrite <- aperm(arrayToWrite, c(1,4,2,3))

  newDim <- actualDim
  newDim[2] <- newDim[2] + dimtowrite[2]
  if(writeStruct && length(namesVariable)>0)
  {
    rhdf5::h5set_extent(fid, datatype, c(newDim))
  }
  rhdf5::h5writeDataset.array(obj = arrayToWrite, fid, datatype, index = indexToWrite)
  rhdf5::H5close()
}


.getNewColumnsName <- function(allStraitments)
{
  areas <- NULL
  links <- NULL
  clusters <- NULL
  districts <- NULL
  for(X in pkgEnv$processDispo$fctname){
    if(get(paste0("allStraitments"))[[X]]){
      areas <- c(areas, pkgEnv$process[[X]]$areas)
      links <- c(links, pkgEnv$process[[X]]$links)
      clusters <- c(clusters, pkgEnv$process[[X]]$clusters)
      districts <- c(districts, pkgEnv$process[[X]]$districts)

    }
  }
  list(areas = areas,
       links = links,
       clusters = clusters,
       districts = districts)
}

.getSelectAlias <- function(allStraitments){
  as.character(pkgEnv$processDispo[pkgEnv$processDispo$fctname%in%
                                     names(which(unlist(allStraitments))),]$trtName)
}


.calcNewColumns <- function(res, allStraitments, timeStep, linkCapacity, mustRun, thermalAvailabilities, opts){

  oldw <- getOption("warn")
  options(warn = -1)


  if(allStraitments$addNetLoad){
    res$areas[,"netLoad" := NULL]
    if("districts" %in%names(res)){
      res$districts <- res$districts[,"netLoad" := NULL]
    }
    res$areas <- addNetLoad(res$areas, ignoreMustRun = !mustRun)
    if("districts" %in%names(res)){
      res$districts <- addNetLoad(res$districts, ignoreMustRun = !mustRun)

    }
  }

  if(allStraitments$addDownwardMargin){
    try({
      res <- addDownwardMargin(res)
    })
  }
  if(allStraitments$addUpwardMargin){
    try({
      res <- addUpwardMargin(res)
    })
  }


  if(allStraitments$addExportAndImport){
    try({
      res$links$loadFactor <- NULL
      res$areas$export <- NULL
      res$areas$import <- NULL
      res <- addExportAndImport(res, addCapacities = linkCapacity, opts = opts)


    })
  }
  if(allStraitments$addLoadFactorLink){
    try({
      res <- addLoadFactorLink(res)
    })
  }

  if(allStraitments$externalDependency){
    try({
      res$areas[,"netLoadRamp" := NULL]
      res$areas[,"netLoad" := NULL]
      if("districts" %in% names(res)){
        res$districts[,"netLoadRamp" := NULL]
        res$districts[,"netLoad" := NULL]
      }

      res <- addNetLoad(res, ignoreMustRun = !mustRun)
    })
    try({
      extDep <- externalDependency(res, timeStep =  timeStep, opts = opts)
    })
  }

  if(allStraitments$loadFactor){
    try({
      loadFactor <- loadFactor(res, timeStep =  timeStep, loadFactorAvailable = thermalAvailabilities, opts = opts)
      idC <- getIdCols(loadFactor)
      res$clusters <- merge(res$clusters, loadFactor, by = idC)
    })
  }


  if(allStraitments$modulation){
    try({
      mod <- modulation(res, timeStep =  timeStep, opts = opts)

      idC <- getIdCols(mod)
      res$clusters <- merge(res$clusters, mod, by = idC)
    })
  }
  if(allStraitments$netLoadRamp){
    try({


      netLoadRamp <- netLoadRamp(res, timeStep = timeStep, ignoreMustRun = !mustRun, opts = opts)
      netLoadRamp <- as.antaresDataList(netLoadRamp)

      if("netLoadRamp" %in% names(netLoadRamp)){
        names(netLoadRamp)[1] <- "areas"
      }

      idC <- getIdCols(netLoadRamp$areas)


      res$areas <- merge(res$areas, netLoadRamp$areas, by = idC)

      if("districts"%in%names(netLoadRamp)){
        #Surplus districts
        idC <- getIdCols(netLoadRamp$districts)
        res$districts <- merge(res$districts, netLoadRamp$districts, by = idC)

      }

    })
  }

  if(allStraitments$externalDependency){
    try({
      if(is.null(res$areas$netLoadRamp)){

        res$areas[,"netLoadRamp" := NULL]
        res$areas[,"netLoad" := NULL]
        if("districts" %in% names(res)){
          res$districts[,"netLoadRamp" := NULL]
          res$districts[,"netLoad" := NULL]
        }
        res <- addNetLoad(res)
      }

    })
    try({
      extDep <- externalDependency(res, timeStep =  timeStep, opts = opts)
      extDep <- as.antaresDataList(extDep)
      if(!names(extDep)[1] %in%c("areas", "districts")){
        names(extDep)[1] <- "areas"
      }


      idC <- getIdCols(extDep$areas)
      res$areas <- merge(res$areas, extDep$areas, by = idC)
      if("districts" %in%names(extDep)){
        idC <- getIdCols(extDep$districts)
        res$districts <- merge(res$districts, extDep$districts, by = idC)
      }


    })
  }

  if(allStraitments$surplus){
    try({
      ##Surplus for areas
      surplus <- surplus(res, timeStep = timeStep, opts = opts)
      idC <- getIdCols(surplus)
      res$areas <- merge(res$areas, surplus, by = idC, all.x = TRUE)
      if("districts"%in%names(res)){
        #Surplus districts
        surplus <- surplus(res, groupByDistrict  = TRUE,  timeStep = timeStep)
        idC <- getIdCols(surplus)
        res$districts <- merge(res$districts, surplus, by = idC, all.x = TRUE)

      }


    })
  }
  if(allStraitments$surplusClusters){
    try({

      surplusClusters <- surplusClusters(res, timeStep =  timeStep,
                                         surplusLastUnit = thermalAvailabilities, opts = opts)

      idC <- getIdCols(surplusClusters)
      res$clusters <- merge(res$clusters, surplusClusters, by = idC)
    })
  }
  options(warn = oldw)
  res
}

#'@export
#'@noRd
.setAliasH5 <- function(){
  sapply(names(pkgEnv$process), function(X){
    tpAlias <- pkgEnv$process[[X]]
    X <- paste0("Out_", X)
    sapply(names(tpAlias), function(Y){
      varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
    })
  })
}



.loadAttributes <- function(fid, timeStep){

  .requireRhdf5_Antares(stopP = TRUE)

  if(rhdf5::H5Lexists(fid, paste0(timeStep, "/attrib")))
  {

    did <- rhdf5::H5Dopen(fid, paste0(timeStep, "/attrib"))
    attrib <- unserialize(charToRaw(rhdf5::H5Dread(did)))
    rhdf5::H5Dclose(did)

    if(!is.null(attrib$opts$linksDef)){
      attrib$opts$linksDef <- data.table(attrib$opts$linksDef)
    }
    if(!is.null(attrib$opts$districtsDef)){
      attrib$opts$districtsDef <- data.table(attrib$opts$districtsDef)
    }
  }else{
    attrib <- NULL
  }
  attrib
}
# library(antaresProcessing)
# library(data.table)
# devtools::load_all(".")
# path <- "D:/Users/titorobe/Desktop/Antares/antaresHdf5"
# opts <- setSimulationPathH5(path)
# addStraitments(opts,addDownwardMargin = TRUE)
# timeStep = "hourly"
# addDownwardMargin = TRUE
# addUpwardMargin = TRUE
# addExportAndImport = TRUE
# addLoadFactorLink = TRUE
# externalDependency = TRUE
# loadFactor = TRUE
# modulation = TRUE
# netLoadRamp = TRUE
# surplus = TRUE
# surplusClusters = TRUE
# opts <- setSimulationPath("D:/Users/titorobe/Desktop/Antares/antaresHdf5", 1)
# mcY = "mcInd"
