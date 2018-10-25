context("Function correctBalance")

opts <- setSimulationPath(studyPath)

test_that("correctBalance get an antaresData in x", {
  expect_error(correctBalance(x = 1e-10))
})

test_that("correctBalance must work with an antaresDataTable areas", {
  myDataAreas <- readAntares(areas = "all", showProgress = FALSE, mcYears = 1)
  myDataAreasCopy <- data.table::copy(myDataAreas)
  expect_true(all.equal(unique(myDataAreasCopy$`ROW BAL.`), 0))
  vecCorect <- as.integer(round(rnorm(69, mean = 100, sd = 5),0))
  myDataAreasCopy[area == "b" & timeId > 2710 & timeId < 2780, `ROW BAL.` := vecCorect]
  expect_false(identical(unique(myDataAreasCopy$`ROW BAL.`), 0))
  correctBalance(x = myDataAreasCopy)
  expect_true(all.equal(unique(myDataAreasCopy$`ROW BAL.`), 0))
  expect_equal(unique(myDataAreasCopy$`ROW BAL.`), 0)
  #check the Balance value
  expect_equal(myDataAreasCopy[area == "b" & timeId > 2710 & timeId < 2780, (BALANCE)],
               myDataAreas[area == "b" & timeId > 2710 & timeId < 2780, (BALANCE)] - vecCorect)
  expect_true("oldBalance" %in% names(myDataAreasCopy))
})

test_that("correctBalance must work with an antaresDataTable districts", {
  myDataDistricts <- readAntares(districts = "all", showProgress = FALSE, mcYears = 1)
  myDataDistrictCopy <- data.table::copy(myDataDistricts)
  expect_true(all.equal(unique(myDataDistrictCopy$`ROW BAL.`), 0))
  vecCorect <- as.integer(round(rnorm(69, mean = 100, sd = 5),0))
  myDataDistrictCopy[district == getDistricts()[1]  & timeId > 2710 & timeId < 2780, `ROW BAL.` := vecCorect]
  expect_false(identical(unique(myDataDistrictCopy$`ROW BAL.`), 0))
  correctBalance(x = myDataDistrictCopy)
  expect_true(all.equal(unique(myDataDistrictCopy$`ROW BAL.`), 0))
  expect_equal(unique(myDataDistrictCopy$`ROW BAL.`), 0)
  #check the Balance value
  expect_equal(myDataDistrictCopy[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, (BALANCE)],
               myDataDistricts[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, (BALANCE)] - vecCorect)

  expect_true("oldBalance" %in% names(myDataDistrictCopy))
})

test_that("correctBalance must work with an antaresDataList", {
  myDataArDi <- readAntares(areas = "all", districts = "all", showProgress = FALSE, mcYears = 1)
  myDataDistrictCopy <- data.table::copy(myDataArDi)
  expect_true(all.equal(unique(myDataDistrictCopy$districts$`ROW BAL.`), 0))
  vecCorect <- as.integer(round(rnorm(69, mean = 100, sd = 5),0))
  myDataDistrictCopy$districts[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, `ROW BAL.` := vecCorect]
  expect_false(identical(unique(myDataDistrictCopy$districts$`ROW BAL.`), 0))
  corBalance <- correctBalance(x = myDataDistrictCopy)
  expect_true(all.equal(unique(corBalance$districts$`ROW BAL.`), 0))
  expect_equal(unique(corBalance$districts$`ROW BAL.`), 0)
  #check the Balance value
  myDataDistrictCopy$districts[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, (BALANCE)]
  expect_equal(myDataDistrictCopy$districts[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, (BALANCE)],
               myDataArDi$district[district == getDistricts()[1] & timeId > 2710 & timeId < 2780, (BALANCE)] - vecCorect)

})

test_that("correctBalance must work with an antaresDataTable links", {
  myDataLinks <- readAntares(links = "all", showProgress = FALSE, mcYears = 1)
  myDataLinksCopy <- data.table::copy(myDataLinks)
  expect_true(all.equal(myDataLinksCopy, correctBalance(x = myDataLinksCopy)))
})

test_that("correctBalance must work with an antaresDataList without areas or districts", {
  myDataLinksClusters <- readAntares(links = "all", clusters = "all", showProgress = FALSE, mcYears = 1)
  myDataLinksClustersCopy <- data.table::copy(myDataLinksClusters)
  expect_true(all.equal(myDataLinksClustersCopy, correctBalance(x = myDataLinksClustersCopy)))
})

test_that("h5 : processing correctBalance when mcYear is set to NULL", {
  .skipFunctionH5(h5file)
  optsH5 <- setSimulationPath(h5file)
  correctBalance(x = optsH5)
  # with a new Study H5 test
  ## create a new folder h5
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }

  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))
  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = "b",
    classData = "areas",
    timeId = 1:100,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = NULL
  )

  optsH5New <- setSimulationPath(path = pathNewH5File)
  myDataB <- readAntares(opts = optsH5New, areas = "b", mcYears = NULL)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 15000)
  correctBalance(x = optsH5New)
  myDataB <- readAntares(opts = optsH5New, areas = "b", mcYears = NULL)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)

})

test_that("h5 : processing correctBalance when mcYear is set to 1", {
  .skipFunctionH5(h5file)
  optsH5 <- setSimulationPath(h5file)
  correctBalance(x = optsH5)
  # with a new Study H5 test
  ## create a new folder h5
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }

  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))

  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = "b",
    classData = "areas",
    timeId = 1:100,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = 1
  )

  optsH5New <- setSimulationPath(path = pathNewH5File)
  myDataB <- readAntares(opts = optsH5New, areas = "b", mcYears = 1)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 15000)
  correctBalance(x = optsH5New)
  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1, select = c("BALANCE", "ROW BAL.", "oldBalance"))
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)
  expect_true("oldBalance" %in% names(myDataB))
})

test_that("h5 : processing correctBalance when mcYear is set to 1 for weekly or monthly data", {
  .skipFunctionH5(h5file)
  optsH5 <- setSimulationPath(h5file)
  correctBalance(x = optsH5)
  # with a new Study H5 test
  ## create a new folder h5
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }

  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))

  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = "b",
    classData = "areas",
    timeId = 1,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = 1,
    timeStep = "weekly"
  )

  optsH5New <- setSimulationPath(path = pathNewH5File)
  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1, timeStep = "weekly")
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 15000)
  correctBalance(x = optsH5New)
  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1,
                         select = c("BALANCE", "ROW BAL.", "oldBalance"),
                         timeStep = "weekly")
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)
  expect_true("oldBalance" %in% names(myDataB))

  # monthly data
  timeStepToTest <- "monthly"
  optsH5 <- antaresRead::setSimulationPath(path = h5file)
  myDataB <- readAntares(opts = optsH5,
                         areas = "b",
                         mcYears = 1, timeStep = timeStepToTest)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }
  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))

  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = "b",
    classData = "areas",
    timeId = 1,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = 1,
    timeStep = timeStepToTest
  )
  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1, timeStep = timeStepToTest)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 15000)
  correctBalance(x = optsH5New)
  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1,
                         timeStep = timeStepToTest,
                         select = c("BALANCE", "ROW BAL.", "oldBalance"))
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)
  expect_true("oldBalance" %in% names(myDataB))
})

test_that("h5 : processing correctBalance when mcYear is set to 1 for monthly data on districts", {
  .skipFunctionH5(h5file)
  # monthly data
  timeStepToTest <- "monthly"
  optsH5 <- antaresRead::setSimulationPath(path = h5file)
  districtToC <- getDistricts()[1]
  myDataC <- readAntares(opts = optsH5,
                         districts = districtToC,
                         mcYears = 1,
                         timeStep = timeStepToTest)
  expect_equal(max(unique(myDataC$`ROW BAL.`)), 0)
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }
  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))
  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))

  pathNewH5File <- "E:\\ANTARES\\Exemple_antares\\1_h5_test\\20170707-1355eco-test.h5"
  optsH5New <- setSimulationPath(path = pathNewH5File)
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = districtToC,
    classData = "districts",
    timeId = 1,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = 1,
    timeStep = timeStepToTest
  )
  myDataC <- readAntares(opts = optsH5New, district = districtToC,
                         mcYears = 1, timeStep = timeStepToTest)
  expect_equal(max(unique(myDataC$`ROW BAL.`)), 15000)
  correctBalance(x = optsH5New)
  myDataC <- readAntares(opts = optsH5New,
                         district = districtToC,
                         mcYears = 1,
                         timeStep = timeStepToTest,
                         select = c("BALANCE", "ROW BAL.", "oldBalance"))
  expect_equal(max(unique(myDataC$`ROW BAL.`)), 0)
  expect_true("oldBalance" %in% names(myDataC))
})

test_that("h5 : processing correctBalance with addProcessingH5", {
  .skipFunctionH5(h5file)
  optsH5 <- setSimulationPath(h5file)
  correctBalance(x = optsH5)
  myData <- readAntares(opts = optsH5, areas = "b", mcYears = 1)
  expect_equal(max(unique(myData$`ROW BAL.`)), 0)
  # with a new Study H5 test
  ## create a new folder h5
  pathInitial <- file.path(dirname(studyPath))
  pathNewH5 <- file.path(pathInitial, "testH5")
  if (!dir.exists(pathNewH5)){
    dir.create(pathNewH5)
  }

  #write a new H5 file
  optsData <- antaresRead::setSimulationPath(path = studyPath)
  suppressWarnings(writeAntaresH5(path = pathNewH5, opts = optsData,
                                  overwrite = TRUE, supressMessages = TRUE))

  pathNewH5File <- file.path(pathNewH5, list.files(pathNewH5))
  .h5Antares_edit_variable(
    pathH5 = pathNewH5File,
    instanceData = "b",
    classData = "areas",
    timeId = 1:100,
    antVar = "ROW BAL.",
    newValue = 15000,
    mcYear = 1
  )

  optsH5New <- setSimulationPath(path = pathNewH5File)
  myDataB <- readAntares(opts = optsH5New, areas = "b", mcYears = 1)
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 15000)

  addProcessingH5(opts = optsH5New,
                  correctBalance = TRUE,
                  mcY = "mcInd")

  myDataB <- readAntares(opts = optsH5New, areas = "b",
                         mcYears = 1, select = c("BALANCE", "ROW BAL.", "oldBalance"))
  expect_equal(max(unique(myDataB$`ROW BAL.`)), 0)

  expect_true("oldBalance" %in% names(myDataB))
})
