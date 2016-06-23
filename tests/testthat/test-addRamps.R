context("Function netLoadRamp")

describe("netLoadRamp", {

  mydata <- readAntares(areas = "all", districts = "all", links = "all",
                        synthesis = FALSE, showProgress = FALSE)

  it("returns an antaresDataTable with correct number of lines and columns", {
    s <- netLoadRamp(mydata$areas, ignoreMustRun = TRUE)
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s) / length(simOptions()$mcYears) / (24 * 7 * 52),
                 nrow(unique(mydata$areas[, .(area)])))
  })


  it("accepts 'antaresDataList' objects", {
    s <- netLoadRamp(mydata, ignoreMustRun = TRUE)

    expect_is(s, "antaresDataList")
    expect_false(is.null(s$areas$areaRamp))
    expect_false(is.null(s$districts$areaRamp))
  })


  it("creates min and max columns only if timeStep is not hourly or synthesis is true", {
    s <- netLoadRamp(mydata$areas, ignoreMustRun = TRUE)
    expect_true(is.null(s$minAreaRamp))

    s <- netLoadRamp(mydata$areas, ignoreMustRun = TRUE, synthesis = TRUE)
    expect_false(is.null(s$minAreaRamp))

    s <- netLoadRamp(mydata$areas, ignoreMustRun = TRUE, timeStep = "monthly")
    expect_false(is.null(s$minAreaRamp))
  })


  it("stops if input does not contain area or district data", {
    expect_error(netLoadRamp(mydata$links, ignoreMustRun = TRUE), "area")
  })


  it("stops if some 'necesary'BALANCE' column is missing", {
    mydata <- readAntares(areas="all", showProgress = FALSE, select = "LOAD", synthesis = FALSE)
    expect_error(netLoadRamp(mydata, ignoreMustRun = TRUE), "missing")
  })

})
