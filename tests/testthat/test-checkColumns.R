context("Function .checkColumns")

opts <- setSimulationPath(studyPath)

dataList <- readAntares(areas="all", links = "all", timeStep = "annual",
                        select = c("LOAD", "CONG. FEE (ABS.)"), showProgress = FALSE)

dataTbl <- readAntares(areas="all", timeStep = "annual",
                       select = "LOAD", showProgress = FALSE)

test_that("It stops if 'x' is not an antares data object", {
  expect_error(.checkColumns(1:10, "LOAD"), "antaresData")
})

test_that("It stops if some column is missing", {
  expect_error(
    .checkColumns(dataList, list(areas = "NUCLEAR", links = "CONG. FEE (ABS.)")),
    "NUCLEAR"
  )
})

test_that("It returns the original object if all columns are present", {
  data2 <- .checkColumns(dataList, list(areas = "LOAD", links = "CONG. FEE (ABS.)"))
  expect_identical(dataList, data2)
})

test_that("It also works with antaresDataTable objects", {
  data2 <- .checkColumns(dataTbl, list(areas = "LOAD"))
  expect_identical(dataTbl, data2$area)
  expect_is(data2, "antaresDataList")
  expect_true(names(data2) == "areas")
})
