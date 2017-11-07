context("Function modulation")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(clusters = "all", showProgress = FALSE, mcYears = "all")
m <- modulation(mydata)

describe("modulation", {

  it("returns an antaresDataTable with correct number of lines and columns", {
    expect_is(m, "antaresDataTable")
    expect_equal(nrow(m) / length(simOptions()$mcYears),
                 nrow(unique(mydata[, .(area, cluster)])))
  })

})

test_that("modulations are positive", {
  expect_true(all(m$meanUpwardModulation >= 0))
  expect_true(all(m$meanDownwardModulation >= 0))
  expect_true(all(m$meanAbsoluteModulation >= 0))
  expect_true(all(m$maxUpwardModulation >= 0))
  expect_true(all(m$maxDownwardModulation >= 0))
  expect_true(all(m$maxAbsoluteModulation >= 0))
})

test_that("Modulation are lower than unit capacity", {
  clusterDesc <- readClusterDesc()
  m <- merge(m, clusterDesc, by = c("area", "cluster"))
  expect_true(all(m$meanUpwardModulation <= m$nominalcapacity))
  expect_true(all(m$meanDownwardModulation <= m$nominalcapacity))
  expect_true(all(m$meanAbsoluteModulation <= m$nominalcapacity))
  expect_true(all(m$maxUpwardModulation <= m$nominalcapacity))
  expect_true(all(m$maxDownwardModulation <= m$nominalcapacity))
  expect_true(all(m$maxAbsoluteModulation <= m$nominalcapacity))
})


mydata <- readAntares(areas = "all", showProgress = FALSE, mcYears = "all")
m <- modulation(mydata, by = "sector")

test_that("modulations by sectors are positive", {
  expect_true(all(m$meanUpwardModulation >= 0))
  expect_true(all(m$meanDownwardModulation >= 0))
  expect_true(all(m$meanAbsoluteModulation >= 0))
  expect_true(all(m$maxUpwardModulation >= 0))
  expect_true(all(m$maxDownwardModulation >= 0))
  expect_true(all(m$maxAbsoluteModulation >= 0))
})


mydata <- readAntares(areas = "all", districts = "all", showProgress = FALSE, mcYears = "all")
m <- modulation(mydata, by = "sector")


test_that("modulations are positive, apply to an antaresDataList", {
  expect_true(all(m$meanUpwardModulation >= 0))
  expect_true(all(m$meanDownwardModulation >= 0))
  expect_true(all(m$meanAbsoluteModulation >= 0))
  expect_true(all(m$maxUpwardModulation >= 0))
  expect_true(all(m$maxDownwardModulation >= 0))
  expect_true(all(m$maxAbsoluteModulation >= 0))
})


mydata <- readAntares(areas = "all", clusters = "all", showProgress = FALSE, mcYears = "all")
m <- modulation(mydata, by = "sector", synthesis = TRUE)


test_that("modulations are positive, apply to an antaresDataList", {
  expect_true(all(m$meanUpwardModulation >= 0))
  expect_true(all(m$meanDownwardModulation >= 0))
  expect_true(all(m$meanAbsoluteModulation >= 0))
  expect_true(all(m$maxUpwardModulation >= 0))
  expect_true(all(m$maxDownwardModulation >= 0))
  expect_true(all(m$maxAbsoluteModulation >= 0))
})

