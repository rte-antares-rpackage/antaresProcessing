context("Surplus of sectors")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas="all", clusters = "all",
                      showProgress = FALSE, mcYears = "all")

s <- surplusSectors(mydata)

describe("surplusSectors", {

  it("returns an antaresDataTable with correct number of lines and columns", {
    expect_is(s, "antaresDataTable")
  })
})
