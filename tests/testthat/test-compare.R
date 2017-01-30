context("Function compare")

opts <- setSimulationPath(studyPath)

describe("compare", {

  data1 <- readAntares(c("a", "b"), select = c("LOAD", "BALANCE"),
                       timeStep = "annual", showProgress = FALSE)

  it("returns an object with same structure than the input", {
    data2 <- readAntares(c("a", "b"), select = c("LOAD", "BALANCE"),
                         timeStep = "annual", showProgress = FALSE)
    res <- compare(data1, data2)

    expect_is(res, "antaresDataTable")
    expect_identical(names(res), names(data1))
  })

  it("keeps only shared rows and columns", {
    data2 <- readAntares(c("b", "c"), select = c("LOAD", "BALANCE", "MRG. PRICE"),
                         timeStep = "annual", showProgress = FALSE)
    res <- compare(data1, data2)
    expect_identical(names(res),
                     intersect(names(data1), names(data2)))
    expect_identical(as.character(unique(res$area)),
                     intersect(data1$area, data2$area))
  })

  it("stops if x and y have different types", {
    data2 <- readAntares(links="all", select = c("FLOW LIN."),
                         timeStep = "annual", showProgress = FALSE)
    expect_error(compare(data1, data2), "type")
  })

  it("returns 0s if x = y", {
    res <- compare(data1, data1)
    expect_true(all(res$LOAD == 0 & res$BALANCE == 0))
  })

})

mydata <- readAntares(c("a", "b"), select = c("LOAD"),
                      timeStep = "annual", showProgress = FALSE)

mydata2 <- copy(mydata)
mydata2[, LOAD := LOAD * 1.2]

test_that("Differences are correctly computed", {
  res <- compare(mydata, mydata2, "diff")
  expect_equal(res$LOAD, mydata$LOAD * 0.2)
})

test_that("Ratios are correctly computed", {
  res <- compare(mydata, mydata2, "ratio")
  expect_true(all(res$LOAD - 1.2 < 1e-10))
})

test_that("Evolutions are correctly computed", {
  res <- compare(mydata, mydata2, "rate")
  expect_true(all(res$LOAD - 0.2 < 1e-10))
})


