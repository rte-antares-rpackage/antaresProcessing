context("Function mergeAllAntaresData")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  mydata <- readAntares( areas = "all",
                         mcYears = "all", showProgress = FALSE)

  describe("mergeAllAntaresData", {
    dimD <- dim(mydata)
    nbDcastCol <- getIdCols(mydata)
    nbDcastCol <- nbDcastCol[nbDcastCol != "area"]
    dimD[2] <- dimD[2] - length(nbDcastCol) - 1
    rowId <- length(nbDcastCol)*dimD[1]/length(unique(mydata$area))
    expect_true(prod(dimD)+rowId==prod(dim(mergeAllAntaresData(mydata))))
  })


  mydata <- readAntares( areas = "all", links = "all",
                         mcYears = "all", showProgress = FALSE)

  describe("mergeAllAntaresData", {
    dDim <- dim(mergeAllAntaresData(mydata))
    nbDcastCol <- getIdCols(mydata[[1]])
    nbDcastCol <- nbDcastCol[nbDcastCol != "area"]

    Nc <- unlist(lapply(mydata, ncol))
    Nc <- Nc - length(nbDcastCol) - 1
    nbAr <- length(unique(mydata$areas$area))
    nbLn <- length(unique(mydata$links$link))
    expect_true(Nc[1] * nbAr +  Nc[2] * nbLn  + length(nbDcastCol) ==   dDim[2])

  })

})

