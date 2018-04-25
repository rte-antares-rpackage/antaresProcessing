context("Surplus of sectors")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas="all", clusters = "all",
                      showProgress = FALSE, mcYears = "all")

s <- suppressWarnings(surplusSectors(mydata))

describe("surplusSectors", {

  it("returns an antaresDataTable with correct number of lines and columns", {
    expect_is(s, "antaresDataTable")
  })
})
