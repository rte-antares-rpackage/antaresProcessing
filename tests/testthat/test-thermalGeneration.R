context("thermalGeneration")


opts <- setSimulationPath(studyPath)

test_that("thermalGeneration calc",{
  Tres <- thermalGeneration(opts)
  reCl <- readClusterDesc(opts)
  uniqueRe <- unique(reCl[, .SD, .SDcols = c("area", "group")])
  expect_true(nrow(uniqueRe) == nrow(Tres))
})

if(requireNamespace("rhdf5")){
  rhdf5::H5close()
}
if(dir.exists(pathtodelete))
{
  unlink(pathtodelete, recursive = TRUE)
}
