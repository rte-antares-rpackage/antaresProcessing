context("Function addRamps")

describe("addRamps", {

  it("adds column 'netLoad' to the input data", {
    mydata <- readAntares("all", timeStep = "monthly", showProgress = FALSE, select = "nostat")
    addRamps(mydata, ignoreMustRun = TRUE)
    expect_false(is.null(mydata$netLoadRamp))
    expect_false(is.null(mydata$balanceRamp))
    expect_false(is.null(mydata$areaRamp))
  })


  it("accepts 'antaresDataList' objects", {
    mydata <- readAntares(areas = "all", districts = "all", timeStep = "monthly", showProgress = FALSE)
    addRamps(mydata, ignoreMustRun = TRUE)

    expect_is(mydata, "antaresDataList")
    expect_false(is.null(mydata$areas$areaRamp))
    expect_false(is.null(mydata$districts$areaRamp))
  })


  it("stops if input does not contain area or district data", {
    mydata <- readAntares(links="all", timeStep = "monthly", showProgress = FALSE)
    expect_error(addRamps(mydata, ignoreMustRun = TRUE), "area")
  })


  it("stops if input already contains ramps columns", {
    mydata <- readAntares(areas="all", timeStep = "monthly", showProgress = FALSE)
    addRamps(mydata, ignoreMustRun = TRUE)
    expect_error(addRamps(mydata, ignoreMustRun = TRUE))
  })

  it("stops if some 'necesary'BALANCE' column is missing", {
    mydata <- readAntares(areas="all", timeStep = "monthly", showProgress = FALSE, select = "LOAD")
    expect_error(addRamps(mydata, ignoreMustRun = TRUE), "missing")
  })

})
