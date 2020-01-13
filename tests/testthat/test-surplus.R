context("Function surplus")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  data <- suppressWarnings(readAntares(areas="all", links = "all", showProgress = FALSE,
                                       linkCapacity = TRUE, mcYears = 1))

  describe("surplus", {

    it("stops if some links are missing", {
      data2 <- readAntares(areas="all", links = "a - b", showProgress = FALSE,
                           mcYears = "all")
      expect_error(surplus(data2), "missing")
    })

    it("stops if time step is not hourly", {
      data2 <- readAntares(areas="all", links = "a - b", timeStep = "annual",
                           showProgress = FALSE, mcYears = "all")
      expect_error(surplus(data2), "hourly")
    })

    it("returns a data.table with correct number of rows and correct columns", {
      surplus <- surplus(data, timeStep = "hourly")
      expect_equal(nrow(surplus), nrow(data$areas))
      expect_true(all(c("area", "timeId", "consumerSurplus",
                        "producerSurplus", "congestionFees", "globalSurplus") %in% names(surplus)))
    })

  })

  test_that("Surpluses are correctly computed", {
    dataCorrected <- removeVirtualAreas(data,
                                        storageFlexibility = getAreas(c("psp", "hub")),
                                        production = getAreas("offshore"))
    surplus <- suppressWarnings(surplus(dataCorrected))

    surplusWithHurdle <- suppressWarnings(surplus(dataCorrected, hurdleCost = FALSE))

    data$areas$production <- data$areas[,`H. ROR` + WIND + SOLAR + NUCLEAR + LIGNITE + COAL +
                                          GAS + OIL + `MIX. FUEL` + `MISC. DTG` + `H. STOR` +
                                          `MISC. NDG`]

    # Consumer surplus
    surplusConsoA <- data$areas[area == "a", sum( (10000 - `MRG. PRICE`) * LOAD)]
    expect_equal(surplusConsoA, surplus[area == "a", consumerSurplus])

    # Producer surplus
    mrgPrice <- data$areas[area == "a", `MRG. PRICE`]
    surplusProdA <- data$areas[area == "a", sum(production * mrgPrice - `OV. COST`)]

    surplusProdA <- surplusProdA + data$areas[area == "a_offshore", sum(production* mrgPrice)]

    expect_equal(surplusProdA, surplus[area == "a", producerSurplus])

    # Storage surplus
    storageSurplusA <- - data$links[link == "a - psp out", sum(mrgPrice * `FLOW LIN.`)] -
      data$links[link == "a - psp in", sum(mrgPrice * `FLOW LIN.`)] +
      data$areas[area == "a", sum(`MRG. PRICE` * PSP)]
    expect_equal(storageSurplusA, surplus[area == "a", storageSurplus])

    # Congestion fees
    congestionFeesA <- 1/2 * data$links[link == "a - b", sum(`CONG. FEE (ALG.)`-`HURDLE COST`)]
    expect_equal(congestionFeesA, surplus[area == "a", congestionFees])

    # Congestion fees with hurdle cost
    congestionFeesAWHC <- 1/2 * data$links[link == "a - b", sum(`CONG. FEE (ALG.)`)]
    expect_equal(congestionFeesAWHC, surplusWithHurdle[area == "a", congestionFees])

    # ROW Balance
    rowSurplusA <- data$areas[area == "a", sum(`MRG. PRICE` * `ROW BAL.`)]
    expect_equal(rowSurplusA, surplus[area == "a", rowBalanceSurplus])

    # Total surplus
    expect_equal(surplusConsoA + surplusProdA + storageSurplusA + congestionFeesA + rowSurplusA,
                 surplus[area == "a", globalSurplus])
  })

  test_that("bug in consumerSurplus, github #38", {
    opts <- setSimulationPath(studyPath)
    data <- suppressWarnings(readAntares(areas="all", links = "all", showProgress = FALSE,
                                         linkCapacity = TRUE, mcYears = 1))
    # get the reference
    expect_equal(opts$energyCosts$unserved[["c"]], 10000)
    resSurplus <- antaresProcessing::surplus(data,
                                             timeStep = "hourly",
                                             opts = opts)
    conSC <- resSurplus[area=="c" & timeId==2690, consumerSurplus]
    expect_equal(conSC, 503967500)
    # change the unserved cost for area c
    opts$energyCosts$unserved[["c"]] <- 0
    data$areas[area=="c", `MRG. PRICE` := 0]
    expect_equal(unique(data$areas[area=="c", `MRG. PRICE`]), 0)
    resSurplus <- antaresProcessing::surplus(data,
                                             timeStep = "hourly",
                                             opts = opts)
    conSC <- resSurplus[area=="c" & timeId==2690, consumerSurplus]
    expect_equal(conSC, 0)
    # verification for area b
    unServedB <- opts$energyCosts$unserved[["b"]]
    data$areas[, verif := (opts$energyCosts$unserved[area] - `MRG. PRICE`)*LOAD]
    expect_equal(resSurplus[area=="b", consumerSurplus], data$areas[area == "b", verif])
    # change the value for area b
    opts$energyCosts$unserved[["b"]] <- 40000
    resSurplus <- antaresProcessing::surplus(data,
                                             timeStep = "hourly",
                                             opts = opts)
    data$areas[, verif := (opts$energyCosts$unserved[area] - `MRG. PRICE`)*LOAD]
    expect_equal(resSurplus[area=="b", consumerSurplus], data$areas[area == "b", verif])
    expect_gt(min(resSurplus[area=="b", consumerSurplus]), max(resSurplus[area=="a", consumerSurplus]))
    # set the initial values
    opts <- setSimulationPath(studyPath)
    data <- suppressWarnings(readAntares(areas="all", links = "all", showProgress = FALSE,
                                         linkCapacity = TRUE, mcYears = 1))
  })

})

