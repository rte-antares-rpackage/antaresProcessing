context("h5 : .aggregateMcYears")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  describe(".aggregateMcYears", {

    mydata <- readAntares(areas = "all", mcYears = "all", showProgress = FALSE)

    .aggregateMcYears(mydata)
  })

})
