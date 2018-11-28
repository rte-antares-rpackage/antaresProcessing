context("Function addConvergencePriceArea")

opts <- setSimulationPath(studyPath)

test_that("addConvergencePriceArea works with an antaresDataList.
          antaresData must contains areas and links details data with linkCapacity.", {
  myData <- readAntares(clusters = "all", showProgress = FALSE)
  expect_error(addConvergencePriceArea(myData))
  myData <- readAntares(areas = "all", showProgress = FALSE)
  expect_error(addConvergencePriceArea(myData))
  myData <- readAntares(areas = "all",
                        links = "all",
                        showProgress = FALSE)
  expect_error(addConvergencePriceArea(myData))
  myData <- readAntares(areas = "all",
                        links = "all",
                        showProgress = FALSE,
                        mcYears = "all")
  expect_error(addConvergencePriceArea(myData))
  myData <- suppressWarnings(readAntares(areas = "all",
                                         links = "all",
                                         showProgress = FALSE,
                                         linkCapacity = TRUE,
                                         mcYears = "all"))
  addConvergencePriceArea(myData)
})

test_that("addConvergencePriceArea add a column priceConvergenceArea
          For each time, priceConvergenceArea represent the system without congestion
          for one area", {
            myData <- suppressWarnings(readAntares(areas = "all",
                                                   links = "all",
                                                   showProgress = FALSE,
                                                   linkCapacity = TRUE,
                                                   mcYears = "all"))
            myDataRV <- removeVirtualAreas(x = myData,
                                           storageFlexibility = getAreas(c("psp", "hub")),
                                           production = getAreas("off"))
            addConvergencePriceArea(myDataRV)

            expect_true("priceConvergenceArea" %in% names(myDataRV$areas))
            expect_true(is.character(class(myDataRV$areas$priceConvergenceArea)))

            #when there is priceConvergenceArea = "a b c" then we have the same prices
            indexAll <- myDataRV$areas[priceConvergenceArea=="a b c", which = TRUE]
            myDataPrice <- myDataRV$areas[indexAll,
                                          .(area, mcYear, timeId, `MRG. PRICE`)]
            IdCols <- getIdCols(myDataPrice)
            #without mcYear
            IdColsWA <- IdCols[IdCols!="area"]
            myFormula <- sprintf("%s ~ area", paste(IdColsWA, collapse = "+"))
            diffPrice <- data.table::dcast(data = myDataPrice,
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
            indexAB <- myDataRV$areas[priceConvergenceArea=="a b", which = TRUE]
            myDataPrice <- myDataRV$areas[indexAB,
                                          .(area, mcYear, timeId, `MRG. PRICE`)]

            diffPrice <- data.table::dcast(data = myDataPrice,
                               as.formula(myFormula),
                               value.var = "MRG. PRICE")
            diffPrice[, ':=' (diffBA = abs(b - a))]
            #diff price not bigger than hurdle cost for A / B but not for C / B
            expect_true(max(diffPrice$diffBA) <= maxDiffNormal)

            #when priceConvergenceArea = "a - b" the area is a or b
            expect_false("c" %in% unique(myDataRV$areas[priceConvergenceArea=="a b",
                                           (area)]))
            expect_true("a" %in% unique(myDataRV$areas[priceConvergenceArea=="a b",
                                           (area)]))
            expect_true("b" %in% unique(myDataRV$areas[priceConvergenceArea=="a b",
                                                       (area)]))

            #when priceConvergenceArea = "a" the area is a
            expect_true("a" %in% unique(myDataRV$areas[priceConvergenceArea=="a",
                                                        (area)]))
            expect_false("b" %in% unique(myDataRV$areas[priceConvergenceArea=="a",
                                                       (area)]))
            expect_false("c" %in% unique(myDataRV$areas[priceConvergenceArea=="a",
                                                       (area)]))

})
