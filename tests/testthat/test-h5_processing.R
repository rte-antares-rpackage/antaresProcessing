context("h5 : addProcessingH5")

if(requireNamespace("rhdf5")){
  if(!is.null(h5file)){
    test_that("h5 : processing, write results", {
      .setAliasH5()
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",
                                        addNetLoad = TRUE,
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

    test_that("h5 : processing, write results mcAll", {
      .setAliasH5()
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcAll",
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


    test_that("h5 : all data", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",
                                        allProcess = TRUE)})
    })

    test_that("h5 : all data multi process", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",
                                        addUpwardMargin = TRUE, nThreads = 2)})
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





    test_that("h5 : processing Out_addNetLoad", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",addNetLoad =  TRUE)})
      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_addNetLoad")
      expect_false(is.null(re$areas$netLoad))
      expect_false(is.null(re$districts$netLoad))

    })


    test_that("h5 : processing Out_addDownwardMargin", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",addDownwardMargin =  TRUE)})
      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                  mcYears = 1, select = "Out_addDownwardMargin")
      expect_false(is.null(re$areas$isolatedDownwardMargin))
      expect_false(is.null(re$areas$interconnectedDownwardMargin))
      expect_false(is.null(re$districts$isolatedDownwardMargin))
      expect_false(is.null(re$districts$interconnectedDownwardMargin))

    })


    test_that("h5 : processing Out_addUpwardMargin", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",addUpwardMargin = TRUE)})
      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_addUpwardMargin")
      expect_false(is.null(re$areas$isolatedUpwardMargin))
      expect_false(is.null(re$areas$interconnectedUpwardMargin))
      expect_false(is.null(re$districts$isolatedUpwardMargin))
      expect_false(is.null(re$districts$interconnectedUpwardMargin))

    })


    test_that("h5 : processing Out_addExportAndImport", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",addExportAndImport = TRUE)})
      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_addExportAndImport")
      expect_false(is.null(re$areas$import))
      expect_false(is.null(re$areas$export))
      expect_false(is.null(re$districts$import))
      expect_false(is.null(re$districts$export))

    })

    test_that("h5 : processing Out_addLoadFactorLink", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",addLoadFactorLink = TRUE)})
      re <- readAntares(opts = optsH5, links = "all",clusters = "all",
                        mcYears = 1, select = "Out_addLoadFactorLink")
      expect_false(is.null(re$links$loadFactor))
      expect_false(is.null(re$links$congestion))




    })


    test_that("h5 : processing Out_externalDependency", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",externalDependency = TRUE)})
      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_externalDependency")
      expect_false(is.null(re$areas$netLoad))
      expect_false(is.null(re$areas$exportsLevel))
      expect_false(is.null(re$areas$importsLevel))
      expect_false(is.null(re$areas$exportsFrequency))
      expect_false(is.null(re$areas$importsFrequency))

      expect_false(is.null(re$districts$netLoad))
      expect_false(is.null(re$districts$exportsLevel))
      expect_false(is.null(re$districts$importsLevel))
      expect_false(is.null(re$districts$exportsFrequency))
      expect_false(is.null(re$districts$importsFrequency))

    })



    test_that("h5 : processing Out_loadFactor", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",loadFactor = TRUE)})
      re <- readAntares(opts = optsH5, clusters = "all",
                        mcYears = 1, select = "Out_loadFactor")
      expect_false(is.null(re$loadFactor))
      expect_false(is.null(re$propHoursMinGen))
      expect_false(is.null(re$propHoursMaxGen))

    })



    test_that("h5 : processing Out_modulation", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd", modulation = TRUE)})


      re <- readAntares(opts = optsH5, clusters = "all",
                        mcYears = 1, select = "Out_modulation")
      expect_false(is.null(re$upwardModulation))
      expect_false(is.null(re$downwardModulation))
      expect_false(is.null(re$absoluteModulation))

    })



    test_that("h5 : processing netLoadRamp", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",netLoadRamp = TRUE)})

      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_netLoadRamp")
      expect_false(is.null(re$areas$netLoadRamp))
      expect_false(is.null(re$areas$balanceRamp))
      expect_false(is.null(re$areas$areaRamp))


      expect_false(is.null(re$districts$netLoadRamp))
      expect_false(is.null(re$districts$balanceRamp))
      expect_false(is.null(re$districts$areaRamp))

    })


    test_that("h5 : processing surplus", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",surplus  = TRUE)})

      re <- readAntares(opts = optsH5, areas = "all", districts = "all",
                        mcYears = 1, select = "Out_surplus")
      expect_false(is.null(re$areas$consumerSurplus))
      expect_false(is.null(re$areas$producerSurplus))
      expect_false(is.null(re$areas$rowBalanceSurplus))
      expect_false(is.null(re$areas$storageSurplus))
      expect_false(is.null(re$areas$congestionFees))
      expect_false(is.null(re$areas$globalSurplus))

      expect_false(is.null(re$districts$consumerSurplus))
      expect_false(is.null(re$districts$producerSurplus))
      expect_false(is.null(re$districts$rowBalanceSurplus))
      expect_false(is.null(re$districts$storageSurplus))
      expect_false(is.null(re$districts$congestionFees))
      expect_false(is.null(re$districts$globalSurplus))

    })

    test_that("h5 : Out_surplusClusters", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings({addProcessingH5(opts = optsH5,  mcY = "mcInd",surplusClusters =  TRUE)})

      re <- readAntares(opts = optsH5, clusters = "all",
                        mcYears = 1, select = "Out_surplusClusters")
      expect_false(is.null(re$variableCost))
      expect_false(is.null(re$fixedCost))
      expect_false(is.null(re$startupCost))
      expect_false(is.null(re$surplusPerUnit))
      expect_false(is.null(re$totalSurplus))
      expect_false(is.null(re$economicGradient))

    })

    test_that("No h5 opts", {
      optsH5 <- setSimulationPath(h5file)
      optsH5$h5path <- NULL
      optsH5$h5 <- NULL
      expect_error(addProcessingH5(optsH5), "opts not refear to an h5 file")

    })

    test_that("Write boolean", {
      optsH5 <- setSimulationPath(h5file)
      suppressWarnings(addProcessingH5(optsH5, evalAreas = list(toto = "TRUE")))
      expect_false(is.null(readAntares(mcYears = 1, select = "toto")$toto))
    })


    test_that("Write character", {
      optsH5 <- setSimulationPath(h5file)
      expect_error(addProcessingH5(optsH5, evalAreas = list(toto = "'GGG'")))
    })


  }
}
