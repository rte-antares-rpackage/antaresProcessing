context("Function getValues")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(clusters = "all", areas = "all",
                      mcYears = "all", showProgress = FALSE)

describe("getValues", {

  it ("stop if data is not a antaresDataTable", {
    expect_error(getValues(data=33), "'data' is not an 'antaresData' object")
  })

  it ("stop if variable is NULL", {
    expect_error(getValues(data=mydata$areas,variable=NULL ), "variable' is NULL")
  })

  it ("stop if variable is not a character", {
    expect_error(getValues(data=mydata$areas,variable=65), "'variable' is not a character")
  })

  it ("check values", {
    res<-getValues(data=mydata$areas,variable="LOAD", mcyear = c(2))
    expect_equal(res[timeId==2689 & area=="a",]$`2`,53709)

    res<-getValues(data=mydata$areas,variable="WIND", mcyear = "all")
    expect_equal(res[timeId==2689 & area=="a",]$`1`,7492)
  })

})
