context("Function addConvergencePriceSystem")

opts <- setSimulationPath(studyPath)

test_that("addConvergencePriceSystem works with an antaresDataList.
          antaresData must contains areas and links details data with linkCapacity.", {
  myData <- readAntares(clusters = "all", showProgress = FALSE)
  expect_error(addConvergencePriceSystem(myData))
  myData <- readAntares(areas = "all", showProgress = FALSE)
  expect_error(addConvergencePriceSystem(myData))
  myData <- readAntares(links = "all", showProgress = FALSE)
  expect_error(addConvergencePriceSystem(myData))

  myData <- readAntares(areas = "all",
                        links = "all",
                        showProgress = FALSE)
  expect_error(addConvergencePriceSystem(myData))
  myData <- suppressWarnings(readAntares(areas = "all",
                        links = "all",
                        showProgress = FALSE,
                        linkCapacity = TRUE))
  expect_error(addConvergencePriceSystem(myData))
  myData <- suppressWarnings(readAntares(areas = "all",
                                         links = "all",
                                         showProgress = FALSE,
                                         linkCapacity = TRUE,
                                         mcYears = "all"))
  addConvergencePriceSystem(myData)
})

test_that("addConvergencePriceSystem add a column priceConvergenceSystem.
          For each time, priceConvergenceSystem represent the system without congestion", {
            myData <- suppressWarnings(readAntares(areas = "all",
                                                   links = "all",
                                                   showProgress = FALSE,
                                                   linkCapacity = TRUE,
                                                   mcYears = "all"))
            myDataRV <- removeVirtualAreas(x = myData,
                                           storageFlexibility = getAreas(c("psp", "hub")),
                                           production = getAreas("off"))
            addConvergencePriceSystem(myDataRV)

            expect_true("priceConvergenceSystem" %in% names(myDataRV$areas))
            expect_true(is.character(class(myDataRV$areas$priceConvergenceSystem)))

            #when there is priceConvergenceSystem = "a b c" then we have the same prices
            indexAll <- myDataRV$areas[priceConvergenceSystem=="a b c", which = TRUE]
            myDataPrice <- myDataRV$areas[indexAll,
                                          .(area, mcYear, timeId, `MRG. PRICE`)]
            IdCols <- getIdCols(myDataPrice)
            #without mcYear
            IdColsWA <- IdCols[IdCols!="area"]
            myFormula <- sprintf("%s ~ area", paste(IdColsWA, collapse = "+"))
            diffPrice <- dcast(data = myDataPrice,
                         as.formula(myFormula),
                         value.var = "MRG. PRICE")
            diffPrice[, ':=' (diffBA = abs(b - a),
                              diffCB = abs(c - b)) ]

            maxHurdl <- max(myDataRV$links[link=="b - c", hurdlesCostDirect])
            maxHurdlInd <- max(myDataRV$links[link=="b - c", hurdlesCostIndirect])
            maxDiffNormal <- max(maxHurdl, maxHurdlInd)
            #diff price not bigger than hurdle cost
            expect_true(max(diffPrice$diffBA) <= maxDiffNormal)
            expect_true(max(diffPrice$diffCB) <= maxDiffNormal)

            # when PriceAreaSystem == "a" then big difference between price
            indexAB <- myDataRV$areas[priceConvergenceSystem=="a b", which = TRUE]
            myDataPrice <- myDataRV$areas[indexAB,
                                          .(area, mcYear, timeId, `MRG. PRICE`)]

            diffPrice <- dcast(data = myDataPrice,
                               as.formula(myFormula),
                               value.var = "MRG. PRICE")
            diffPrice[, ':=' (diffBA = abs(b - a),
                              diffCB = abs(c - b)) ]
            #diff price not bigger than hurdle cost for A / B but not for C / B
            expect_true(max(diffPrice$diffBA) <= maxDiffNormal)
            expect_false(max(diffPrice$diffCB) <= maxDiffNormal)
})
