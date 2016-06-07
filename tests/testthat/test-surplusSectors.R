context("Surplus of sectors")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas="all", clusters = "all",
                      showProgress = FALSE, synthesis = FALSE)

s <- surplusSectors(mydata)

describe("surplusSectors", {

  it("returns an antaresDataTable with correct number of lines and columns", {
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s) / length(simOptions()$mcYears),
                 nrow(unique(mydata$areas[, .(area)])))
  })

  it("fills missing values with 0s", {
    expect_false(any(is.na(s$surplusLignite)))
  })

})
