context("h5 : .aggregateMcYears")

opts <- setSimulationPath(studyPath)

describe(".aggregateMcYears", {

  mydata <- readAntares(areas = "all", mcYears = "all", showProgress = FALSE)

.aggregateMcYears(mydata)
})
