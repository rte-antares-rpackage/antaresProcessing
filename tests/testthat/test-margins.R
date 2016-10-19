context("Function margins")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

describe("margins", {
  mydata <- suppressWarnings({
    readAntares(areas = "all", clusters = "all", districts = "all",
                mustRun = TRUE,
                thermalAvailabilities = TRUE,
                hydroStorageMaxPower = TRUE,
                showProgress = FALSE, timeStep = "annual")
    })

  areasOnly <- mydata
  areasOnly$districts <- NULL

  districtsOnly <- mydata
  districtsOnly$areas <- NULL

  it("returns an antaresDataTable if there is only areas or districts in 'x'", {
    s <- margins(areasOnly)
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s), length(unique(areasOnly$areas$area)))

    s <- margins(districtsOnly)
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s), length(unique(districtsOnly$districts$district)))
  })

  it("It returns an antaresDataList if there is area and district data in 'x'", {
    s <- margins(mydata)
    expect_is(s, "antaresDataList")
  })

})

