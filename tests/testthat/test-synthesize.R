context("Function synthesize")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(areas = "all", links = "all", timeStep = "annual",
                      mcYears = "all", showProgress = FALSE,
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
    expect_equal(attr(sumDataList, "synthesis"), TRUE)
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
    sumdata <- synthesize(mydata$areas, log = list(fun = function(x) mean(log(1+x))))
    expect_true("log_NUCLEAR" %in% names(sumdata))
    expect_true("log_LIGNITE" %in% names(sumdata))
  })

  it("can compute custom statistics for only some columns", {
    sumdata <- synthesize(mydata$areas, log = list(fun = function(x) mean(log(1+x)),
                                                   only = "LIGNITE"))
    expect_false("log_NUCLEAR" %in% names(sumdata))
    expect_true("log_LIGNITE" %in% names(sumdata))

    sumdata <- synthesize(mydata, log = list(fun = function(x) mean(log(1+x)),
                                             only = "LIGNITE"))
    expect_false("log_NUCLEAR" %in% names(sumdata$areas))
    expect_true("log_LIGNITE" %in% names(sumdata$areas))

    # Check that averages are still computed for links
    expect_equal(nrow(sumdata$links), nrow(mydata$links) / length(opts$mcYears))

  })

  it ("skips non numeric variables", {
    mydata$areas[, charColumn := sample(c("a", "b"), .N, TRUE)]
    expect_silent(sumdata <- synthesize(mydata))
    expect_false("chartColumn" %in% names(sumdata$areas))
    mydata$areas[, charColumn := NULL]
  })

  it ("synthesizes logical values", {
    mydata$areas[, boolColumn := sample(c(TRUE, FALSE), .N, TRUE)]
    expect_silent(sumdata <- synthesize(mydata))
    expect_true("boolColumn" %in% names(sumdata$areas))
    mydata$areas[, boolColumn := NULL]
  })

})

test_that("Using synthesize() or import synthetic data should be equivalent", {
  importedSumData <- readAntares(areas = "all", links = "all", timeStep = "annual", showProgress = FALSE,
                                 select = c("NUCLEAR", "LIGNITE",
                                            "FLOW LIN.", "UCAP LIN."))
  sumData <- synthesize(mydata)

  # NOTE: sometimes, the rounding performed by Antares is wrong. So we test that
  # the absolute difference is less or equal to 1.
  expect_true(all(abs(sumData$areas$LIGNITE - importedSumData$areas$LIGNITE) <= 1))
  expect_true(all(abs(sumData$links$`FLOW LIN.` - importedSumData$links$`FLOW LIN.`) <= 1))
  expect_true(all(abs(sumData$links$`UCAP LIN.` - importedSumData$links$`UCAP LIN.`) <= 1))

})
