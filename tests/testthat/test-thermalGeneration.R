context("termalGeneration")


opts <- setSimulationPath(studyPath)

test_that("termalGeneration calc",{
  Tres <- termalGeneration(opts)
  reCl <- readClusterDesc(opts)
  uniqueRe <- unique(reCl[, .SD, .SDcols = c("area", "group")])
  expect_true(nrow(uniqueRe) == nrow(Tres))
})
