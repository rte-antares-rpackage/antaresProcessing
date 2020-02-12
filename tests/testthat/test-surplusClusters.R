context("Surplus of clusters")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  data <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                      showProgress = FALSE, mcYears = "all",
                      thermalAvailabilities = TRUE)

  describe("surplusClusters", {

    it("stops if synthetic or not hourly results", {
      tmp <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                         showProgress = FALSE,
                         thermalAvailabilities = TRUE)
      expect_error(surplusClusters(tmp), "synthetic")

      tmp <- readAntares(areas="all", clusters = "all", select = "MRG. PRICE",
                         showProgress = FALSE, mcYears = "all", timeStep = "annual",
                         thermalAvailabilities = TRUE)
      expect_error(surplusClusters(tmp), "hourly")
    })

    it("returns an antaresDataTable with correct number of lines and columns", {
      s <- suppressWarnings(surplusClusters(data))
      expect_equal(nrow(s) / length(simOptions()$mcYears),
                   nrow(unique(data$clusters[, .(area, cluster)])))
    })

  })

  test_that("All cluster surpluses are positive, except for must run clusters", {
    s <- suppressWarnings(surplusClusters(data))
    s <- s[!cluster %like% "must_run"]
    for (v in c("surplusPerUnit", "totalSurplus")) {
      expect_true(all(s[[v]] >= 0), info = v)
    }
  })

})

