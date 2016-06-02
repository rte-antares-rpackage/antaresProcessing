context("Surplus of clusters")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

data <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                    showProgress = FALSE, synthesis = FALSE)

describe("surplusClusters", {

  it("stops if synthetic or not hourly results", {
    tmp <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                       showProgress = FALSE, synthesis = TRUE)
    expect_error(surplusClusters(tmp), "synthetic")

    tmp <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                       showProgress = FALSE, synthesis = FALSE, timeStep = "annual")
    expect_error(surplusClusters(tmp), "hourly")
  })

  it("returns an antaresDataTable with correct number of lines and columns", {
    s <- surplusClusters(data)
    expect_equal(nrow(s) / length(simOptions()$mcYears),
                 nrow(unique(data$clusters[, .(area, cluster)])))
  })

})

test_that("All cluster surpluses are positive, except for must run clusters", {
  s <- surplusClusters(data)
  s <- s[!cluster %like% "must_run"]
  for (v in c("surplusPerUnit", "surplusLastUnit", "totalSurplus")) {
    expect_true(all(s[[v]] >= 0), info = v)
  }
})
