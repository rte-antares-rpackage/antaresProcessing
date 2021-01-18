context("addCongestionLink")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  dt <- readAntares(links = "all")
  dt <- addCongestionLink(dt)



  expect_true(all(c("congestionFrequencyDirect", "congestionFrequencyIndirect", "congestionHoursDirect",
  "congestionHoursIndirect") %in%  names(dt)))

  expect_error(addCongestionLink(data.table()))
  expect_error(addCongestionLink(dt[, .SD, .SDcols = 1]))

})
