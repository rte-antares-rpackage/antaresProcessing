context("Surplus of sectors")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

data <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                    showProgress = FALSE, synthesis = FALSE)

describe("surplusSectors", {
  it("does stuff", {

  })
})
