context("Function neighbours")

opts <- setSimulationPath(studyPath)

test_that("neighbours get neighbours of an area", {
  res <- antaresProcessing::neighbours(areas = c("a", "c"),
                                       virtualAreas = getAreas("psp"))
  expect_true(grepl("a_offshore", x =  res[area=="a"]$neighbours))
  expect_true(grepl("b", x =  res[area=="a"]$neighbours))
  expect_true(grepl("b", x =  res[area=="c"]$neighbours))
})


test_that("addNeighbours add neighbours to antaresData", {
  myData <- antaresRead::readAntares(links = getLinks("c"), showProgress = FALSE)
  expect_error(addNeighbours(myData))
  myData <- antaresRead::readAntares(areas = c("a", "c"), showProgress = FALSE)
  addNeighbours(myData)
  expect_true("neighbours" %in% names(addNeighbours(myData)))
  neiC <- unique(myData[area=="c", (neighbours)])
  expect_true(grepl("b", neiC))
  expect_true(grepl("hub", neiC))
  myData <- antaresRead::readAntares(areas = c("a", "c"),
                                     links = getLinks("a"),
                                     showProgress = FALSE)
  addNeighbours(myData)
  neiA <- unique(myData$areas[area=="a", (neighbours)])
  expect_true(grepl("b", neiA))
  expect_true(grepl("a_offshore", neiA))
})

test_that("getAllNeighbours get all neighbours", {
  res <- getAllNeighbours(areasString = "a b")
  expect_true("a_offshore" %in% res)
  expect_true("c" %in% res)
})

