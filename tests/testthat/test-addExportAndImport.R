context("Function addExportAndImport")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(clusters = "all", areas = "all",
                      mcYears = "all", showProgress = FALSE)

describe("addExportAndImport", {

  it ("stop if x is not a antaresDataTable", {
    expect_error(addExportAndImport(x=33), "'x' is not an 'antaresData' object")
  })

  it ("stop if links are missing", {
    expect_error(addExportAndImport(x=mydata), "The following links are needed but missing: a - a_offshore, a - b, a - psp in, a - psp out, b - c, b - psp in, b - psp out, c - hub, hub - psp in-2, hub - psp out-2")
  })

  it ("check values for areas and distric", {
    mydata <- readAntares(clusters = "all", areas = "all",
                          mcYears = "all", showProgress = FALSE, links = "all")
    res<-addExportAndImport(x=mydata)
    expect_equal(res$areas[area=="a",]$export[2],3000)
    expect_equal(res$areas[area=="c",]$export[2],0)

    mydata <- suppressWarnings(readAntares(select = "exportsImports", showProgress = FALSE, districts = "all"))

    res<-addExportAndImport(x=mydata, addCapacities = TRUE)
    expect_equal(res$districts[district=="a and b",]$export[2],1500)
    expect_equal(res$districts[district=="a and b",]$export[5],1500)

  })

  it ("stop if x already contains column 'export' and 'import", {
    mydata <- suppressWarnings(readAntares(select = "exportsImports", showProgress = FALSE))

    res<-addExportAndImport(x=mydata, addCapacities=FALSE)

    expect_error(res<-addExportAndImport(x=mydata), "Input already contains column 'export' and 'import'")
  })

  it ("stop if x does not contain transCapacityDirect or transCapacityIndirect data", {

    mydata <- readAntares(areas = "all",
                          mcYears = "all", showProgress = FALSE, links = "all", districts = "all", select = "economy")
    expect_error(res<-addExportAndImport(x=mydata, addCapacities = TRUE), "does not contain transCapacityDirect or transCapacityIndirect data")
  })

})
