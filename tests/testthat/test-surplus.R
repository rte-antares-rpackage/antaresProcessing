context("Surplus function")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

data <- readAntares(areas="all", links = "all", showProgress = FALSE)

describe("surplus", {

  it("stops if some links are missing", {
    data2 <- readAntares(areas="all", links = "a - b", showProgress = FALSE)
    expect_error(surplus(data2), "missing")
  })

  it("stops if time step is not hourly", {
    data2 <- readAntares(areas="all", links = "a - b", timeStep = "annual", showProgress = FALSE)
    expect_error(surplus(data2), "hourly")
  })

  it("returns a data.table with correct number of rows and correct columns", {
    surplus <- surplus(data, timeStep = "hourly")
    expect_equal(nrow(surplus), nrow(data$areas))
    expect_true(all(c("area", "timeId", "consumerSurplus",
                  "producerSurplus", "congestionFees", "globalSurplus") %in% names(surplus)))
  })

  it("also works when synthesis = FALSE", {
    dataDet <- readAntares(areas="all", links = "all", synthesis = FALSE,
                           showProgress = FALSE)
    surplus <- surplus(dataDet, timeStep = "hourly")
    expect_equal(nrow(surplus), nrow(dataDet$areas))
    expect_true(all(c("area", "timeId", "mcYear", "consumerSurplus",
                  "producerSurplus", "congestionFees", "globalSurplus") %in% names(surplus)))
  })

})

test_that("Surpluses are correctly computed", {
  dataCorrected <- removeVirtualAreas(data,
                                      storageFlexibility = getAreas(c("psp", "hub")),
                                      production = getAreas("offshore"))
  surplus <- surplus(dataCorrected)

  data$areas$production <- data$areas[,`H. ROR` + WIND + SOLAR + NUCLEAR + LIGNITE + COAL +
                                        GAS + OIL + `MIX. FUEL` + `MISC. DTG` + `H. STOR`]

  # Consumer surplus
  surplusConsoA <- data$areas[area == "a", sum( (10000 - `MRG. PRICE`) * LOAD)]
  expect_equal(surplusConsoA, surplus[area == "a", consumerSurplus])

  # Producer surplus
  mrgPrice <- data$areas[area == "a", `MRG. PRICE`]
  surplusProdA <- data$areas[area == "a", sum(production * mrgPrice - `OV. COST`)]

  surplusProdA <- surplusProdA + data$areas[area == "a_offshore", sum(production* mrgPrice)]
  surplusProdA <- surplusProdA - data$links[link == "a - psp out", sum(mrgPrice * `FLOW LIN.`)]
  surplusProdA <- surplusProdA - data$links[link == "a - psp in", sum(mrgPrice * `FLOW LIN.`)]

  expect_equal(surplusProdA, surplus[area == "a", producerSurplus])

  # Congestion fees
  congestionFeesA <- 1/2 * data$links[link == "a - b", sum(`CONG. FEE (ALG.)`)]
  expect_equal(congestionFeesA, surplus[area == "a", congestionFees])

  # Total surplus
  expect_equal(surplusConsoA + surplusProdA + congestionFeesA, surplus[area == "a", globalSurplus])
})

