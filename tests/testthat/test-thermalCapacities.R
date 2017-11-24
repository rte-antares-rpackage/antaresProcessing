context("thermalGeneration")


opts <- setSimulationPath(studyPath)

test_that("thermalGeneration calc",{
  Tres <- thermalGeneration(opts)
  reCl <- readClusterDesc(opts)
  uniqueRe <- unique(reCl[, .SD, .SDcols = c("area", "group")])
  expect_true(nrow(uniqueRe) == nrow(Tres))
})
