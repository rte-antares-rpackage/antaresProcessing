context("h5 : addProcessingH5")

if(requireNamespace("rhdf5")){
  if(!is.null(h5file)){
    test_that("h5 : processing, write results", {
      .setAliasH5()
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",
                                        addDownwardMargin = TRUE,
                                        addUpwardMargin = TRUE,
                                        addExportAndImport = TRUE,
                                        addLoadFactorLink = TRUE,
                                        externalDependency = TRUE,
                                        loadFactor = TRUE,
                                        modulation = TRUE,
                                        netLoadRamp = TRUE,
                                        surplus = TRUE,
                                        surplusClusters = TRUE,
                                        evalAreas = list(Tota = "`H. STOR` + `MISC. DTG`",
                                                         Tota2 = "`NODU` + `NP COST` + 1"),
                                        evalLinks = list(),
                                        evalClusters = list(),
                                        evalDistricts = list())})
    })



    test_that("h5 : processing calc by user", {
      optsH5 <- setSimulationPath(h5file)
      calcData <- readAntares(areas = "all", mcYears = "all",
                              select = c("H. STOR" , "MISC. DTG",
                                         "NODU" , "NP COST", "Tota", "Tota2"), opts = optsH5)

      calcData[,verif1 := `H. STOR` + `MISC. DTG`]
      calcData[,verif2 := `NODU` + `NP COST` + 1]
      expect_true(max(calcData$Tota-calcData$verif1) == 0)
      expect_true(max(calcData$Tota2-calcData$verif2) == 0)

    })

    test_that("h5 : processing calc by straitements", {

      UpwardMargin_out <- readAntares(areas = "all", mcYears = "all",
                                      select = "Out_addUpwardMargin")

      UpwardMargin_recalc <- readAntares(areas = "all", mcYears = "all",select = "upwardMargin")
      addUpwardMargin(UpwardMargin_recalc)

      expect_true(identical(UpwardMargin_out$interconnectedUpwardMargin,
                            UpwardMargin_recalc$areas$interconnectedUpwardMargin))

      expect_true(identical(UpwardMargin_out$isolatedUpwardMargin,
                            UpwardMargin_recalc$areas$isolatedUpwardMargin))

    })

    test_that("h5 : processing calc links clusters discticcts", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",
                                        evalLinks = list(totalLink= "`FLOW LIN.` + 10000 "),
                                        evalClusters = list(prodNodu= "`production`*`NODU` "),
                                        evalDistricts = list(prodDic= "`LOAD`*`WIND`"))})
    })

    test_that("h5 : processing all straitements", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",allData = TRUE)})
    })
    test_that("h5 : processing parallel", {
    addProcessingH5(opts = opts,  mcY = "mcInd",
                    allData = TRUE,
                    nThreads =  2
    )})

  }
}
