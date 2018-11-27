context("Function understandThePrice")

opts <- setSimulationPath(studyPath)

mydata <- readAntares(clusters = "all", areas = "all",
                      mcYears = "all", showProgress = FALSE)

nameFinal <- c("marginalClusterArea", "marginalGroupArea", "marBidMarginalCluArea")

test_that("understandThePrice works with an hourly detail antaresData with areas and clusters", {
  #an antaresData
  expect_error(understandThePrice(antaresData = 1e-10),
               regexp = NULL)
  #an detail antaresData
  myDataS <- readAntares(showProgress = FALSE)
  expect_error(understandThePrice(antaresData = myDataS),
               regexp = NULL)
  #an detail antaresData with areas and clusters (here with links)
  myDataS <- readAntares(showProgress = FALSE,
                         mcYears = "all",
                         links = "all",
                         areas = "all")
  expect_error(understandThePrice(antaresData = myDataS),
               regexp = "antaresData does not contain areas, clusters and links data")
  #an detail antaresData with areas and clusters (here only with areas)
  myDataS <- readAntares(showProgress = FALSE,
                         mcYears = "all",
                         areas = "all")
  expect_error(understandThePrice(antaresData = myDataS),
               regexp = "antaresData must be an antaresDataList")
  #an detail antaresData with areas and clusters (no links)
  myDataS <- readAntares(showProgress = FALSE,
                         mcYears = "all",
                         areas = "all",
                         clusters = "all")
  expect_error(understandThePrice(antaresData = myDataS),
              regexp = "antaresData does not contain areas, clusters and links data")
  #an detail antaresData with areas and clusters and links (no linkCapacity)
  myDataS <- readAntares(showProgress = FALSE,
                         mcYears = "all",
                         areas = "all",
                         clusters = "all",
                         links = "all")
  expect_error(understandThePrice(antaresData = myDataS),
               regexp = "antaresData links must contains transCapacityDirect and transCapacityIndirect.")
  #an detail antaresData with areas and clusters and links (but no thermalModulation)
  myDataS <- suppressWarnings(readAntares(showProgress = FALSE,
                         mcYears = "all",
                         areas = "all",
                         clusters = "all",
                         links = "all",
                         linkCapacity = TRUE))
  expect_error(understandThePrice(antaresData = myDataS),
               regexp = "antaresData clusters must contains minGenModulation.")
  #an detail antaresData with areas, clusters and links
  myDataS <- suppressWarnings(readAntares(showProgress = FALSE,
                                          mcYears = "all",
                                          areas = "all",
                                          clusters = "all",
                                          links = "all",
                                          linkCapacity = TRUE,
                                          thermalModulation = TRUE))
  understandThePrice(antaresData = myDataS)
  # test with removeVirtualAreas
  myDataS <- suppressWarnings(readAntares(showProgress = FALSE,
                                          mcYears = "all",
                                          areas = "all",
                                          clusters = "all",
                                          links = "all",
                                          linkCapacity = TRUE,
                                          thermalModulation = TRUE))
  myDataRS <- antaresRead::removeVirtualAreas(myDataS,
                                              storageFlexibility = getAreas(select = c("psp", "hub")),
                                              production = getAreas(select = c("off")))
  understandThePrice(antaresData = myDataRS)
})

test_that("understandThePrice determines the cost of the last cluster without hydro",{
  myDataS <- suppressWarnings(readAntares(showProgress = FALSE,
                                          mcYears = "all",
                                          areas = "all",
                                          clusters = "all",
                                          links = "all",
                                          linkCapacity = TRUE,
                                          thermalModulation = TRUE))
  myDataRS <- antaresRead::removeVirtualAreas(myDataS,
                                              storageFlexibility = getAreas(select = c("psp", "hub")),
                                              production = getAreas(select = c("off")))
  understandThePrice(antaresData = myDataRS)
  #when reasonPrice is not na then we know about the cluster marginal
  expect_true(dim(myDataRS$areas[!is.na(reasonPrice) & is.na(marginalGroup)])[1]==0)

  myDataRS$areas[marBidMarginalCluster!=`MRG. PRICE`][1]

  maxHurdl <- max(myDataRS$links[link=="a - b", hurdlesCostDirect])
  maxHurdlInd <- max(myDataRS$links[link=="a - b", hurdlesCostIndirect])
  maxDiffNormal <- max(maxHurdl, maxHurdlInd)

  #when there is no hydro and flex then price is made by clusters
  expect_true(dim(myDataRS$areas[marBidMarginalCluster > (`MRG. PRICE` + maxDiffNormal*2) &
                   prodFlexConvergeArea == FALSE & prodHydroConvergeArea==FALSE])[1]==0)
  expect_true(dim(myDataRS$areas[marBidMarginalCluster < (`MRG. PRICE` - maxDiffNormal*2) &
                                   prodFlexConvergeArea == FALSE & prodHydroConvergeArea==FALSE])[1]==0)
})
