context("Function synthesize")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", links = "all", timeStep = "annual",
                      synthesis = FALSE, showProgress = FALSE,
                      select = c("NUCLEAR", "LIGNITE",
                                 "FLOW LIN.", "UCAP LIN."))

describe("synthesize", {

  it("returns an object with same structure as 'x'", {
    sumdata <- synthesize(mydata$areas)
    expect_is(sumdata, "antaresDataTable")
    expect_equal(names(sumdata), setdiff(names(mydata$areas), "mcYear"))

    sumDataList <- synthesize(mydata)
    expect_is(sumDataList, "antaresDataList")
    expect_equal(names(sumDataList), names(mydata))
  })

  it("returns synthesized data", {
    sumdata <- synthesize(mydata$areas)
    expect_equal(nrow(sumdata), nrow(mydata$areas) / length(opts$mcYears))
    expect_equal(attr(sumdata, "synthesis"), TRUE)
  })

  it("computes custom variables asked using aliases", {
    sumdata <- synthesize(mydata$areas, "min", "max")
    expect_true(all(c("min_NUCLEAR", "max_NUCLEAR", "min_LIGNITE", "max_LIGNITE") %in% names(sumdata)))
  })

  it("computes custom variables asked using functions", {
    sumdata <- synthesize(mydata$areas, log = function(x) mean(log(1+x)))
    expect_true(all(c("log_NUCLEAR", "log_LIGNITE") %in% names(sumdata)))
  })

  it("computes custom variables asked using lists", {
    sumdata <- synthesize(mydata$areas, log = list(fun = function(x) mean(log(1+x)),
                                                   only = "NUCLEAR"))
    expect_true("log_NUCLEAR" %in% names(sumdata))
    expect_false("log_LIGNITE" %in% names(sumdata))
  })

})

test_that("Using synthesize() or import synthetic data should be equivalent", {
  importedSumData <- readAntares(areas = "all", links = "all", timeStep = "annual",
                                 synthesis = TRUE, showProgress = FALSE,
                                 select = c("NUCLEAR", "LIGNITE",
                                            "FLOW LIN.", "UCAP LIN."))
  sumData <- synthesize(mydata)

  # NOTE: sometimes, the rounding performed by Antares is wrong. So we test that
  # the absolute difference is less or equal to 1.
  expect_true(all(abs(sumData$areas$LIGNITE - importedSumData$areas$LIGNITE) <= 1))
  expect_true(all(abs(sumData$links$`FLOW LIN.` - importedSumData$links$`FLOW LIN.`) <= 1))
  expect_true(all(abs(sumData$links$`UCAP LIN.` - importedSumData$links$`UCAP LIN.`) <= 1))

})
