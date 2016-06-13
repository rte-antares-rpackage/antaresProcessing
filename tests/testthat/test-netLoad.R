context("Net Load")

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

describe("netLoad", {

  it("adds column 'netLoad' to the input data", {
    mydata <- readAntares("all", timeStep = "annual", showProgress = FALSE)
    cols <- names(copy(mydata))
    addNetLoad(mydata, ignoreMustRun = TRUE)
    expect_identical(c(cols, "netLoad"), names(mydata))
  })


  it("accepts 'antaresDataList' objects", {
    mydata <- readAntares(areas = "all", districts = "all", timeStep = "annual", showProgress = FALSE)
    addNetLoad(mydata, ignoreMustRun = TRUE)

    expect_is(mydata, "antaresDataList")
    expect_false(is.null(mydata$areas$netLoad))
    expect_false(is.null(mydata$districts$netLoad))
  })


  it("stops if input does not contain area or district data", {
    mydata <- readAntares(links="all", timeStep = "annual", showProgress = FALSE)
    expect_error(addNetLoad(mydata, ignoreMustRun = TRUE), "area")
  })


  it("stops if input already contains a column 'addNetLoad'", {
    mydata <- readAntares(areas="all", timeStep = "annual", showProgress = FALSE)
    addNetLoad(mydata, ignoreMustRun = TRUE)
    expect_error(addNetLoad(mydata, ignoreMustRun = TRUE))
  })

  it("stops if some necesary column is missing", {
    mydata <- readAntares(areas="all", timeStep = "annual", showProgress = FALSE, select = "LOAD")
    expect_error(addNetLoad(mydata, ignoreMustRun = TRUE), "missing")
  })

})
