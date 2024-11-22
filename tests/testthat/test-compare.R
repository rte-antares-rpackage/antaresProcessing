# context("Function compare")
#
# sapply(studyPathS, function(studyPath){
#
#   opts <- setSimulationPath(studyPath)
#
#   describe("compare", {
#
#     data1 <- readAntares(c("a", "b"), select = c("LOAD", "BALANCE"),
#                          timeStep = "annual", showProgress = FALSE)
#
#     it("returns an object with same structure than the input", {
#       data2 <- readAntares(c("a", "b"), select = c("LOAD", "BALANCE"),
#                            timeStep = "annual", showProgress = FALSE)
#       res <- antaresProcessing::compare(data1, data2)
#
#       expect_is(res, "antaresDataTable")
#       expect_identical(names(res), names(data1))
#     })
#
#     it("keeps only shared rows and columns", {
#       data2 <- readAntares(c("b", "c"), select = c("LOAD", "BALANCE", "MRG. PRICE"),
#                            timeStep = "annual", showProgress = FALSE)
#       res <- antaresProcessing::compare(data1, data2)
#       expect_identical(names(res),
#                        intersect(names(data1), names(data2)))
#       expect_identical(as.character(unique(res[["area"]])),
#                        intersect(data1[["area"]], data2[["area"]]))
#     })
#
#     it("stops if x and y have different types", {
#       data2 <- readAntares(links = "all", select = c("FLOW LIN."),
#                            timeStep = "annual", showProgress = FALSE)
#       expect_error(antaresProcessing::compare(data1, data2), "type")
#     })
#
#     it("returns 0s if x = y", {
#       res <- antaresProcessing::compare(data1, data1)
#       expect_true(all(res$LOAD == 0 & res$BALANCE == 0))
#     })
#
#   })
#
#   mydata <- readAntares(c("a", "b"), select = c("LOAD"),
#                         timeStep = "annual", showProgress = FALSE)
#
#   mydata2 <- copy(mydata)
#   mydata2 <- mydata2[, LOAD := LOAD * 1.2]
#
#   test_that("Differences are correctly computed", {
#     res <- antaresProcessing::compare(mydata, mydata2, "diff")
#     expect_equal(res$LOAD, mydata$LOAD * 0.2)
#   })
#
#   test_that("Ratios are correctly computed", {
#     res <- antaresProcessing::compare(mydata, mydata2, "ratio")
#     expect_true(all(res$LOAD - 1.2 < 1e-10))
#   })
#
#   test_that("Evolutions are correctly computed", {
#     res <- antaresProcessing::compare(mydata, mydata2, "rate")
#     expect_true(all(res$LOAD - 0.2 < 1e-10))
#   })
#
#   test_that("if x = y, then the res is idential to x",
#             {
#               data1 <- readAntares(areas = c("a", "b"), select = c("LOAD", "BALANCE"),
#                                    timeStep = "hourly", showProgress = FALSE)
#               data2 <- readAntares(areas = c("a", "b"), select = c("LOAD", "BALANCE"),
#                                    timeStep = "hourly", showProgress = FALSE)
#
#               expect_true(identical(data1, data2))
#               res <- antaresProcessing::compare(data1, data2)
#               for (t in names(attributes(data1))) {
#                 if (class(attr(data1, t)) != "externalptr" & t != "type" & t != "opts"){
#                   expect_true(all(attr(data1, t) == attr(res, t)),
#                               paste0("attr(data1, t) : ", attr(data1, t), " != ", "attr(res, t) : ", attr(res, t) ))
#                 }
#               }
#             })
#
#   # x can be an antaresDataList (y also)
#   # attr res = attr x whan x = y
#
#   test_that("x and y can be antaresDataList ", {
#     data1 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       links = getLinks(),
#       districts = getDistricts(),
#       clusters = getAreas(),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     data2 <- suppressWarnings(readAntares(
#       areas = c("a", "c"),
#       links = getLinks(exclude = getAreas("off")),
#       districts = getDistricts(),
#       clusters = getAreas(exclude = "b"),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     res <- antaresProcessing::compare(data1, data2)
#     #check the result
#     expect_false(is.null(res$areas))
#     expect_false(is.null(res$links))
#     expect_true("a" %in% unique(res$areas$area))
#     expect_false("b" %in% unique(res$areas$area))
#     expect_false("a - a_offshore" %in% unique(res$links$link))
#     expect_true("a and b" %in% unique(res$districts$district))
#     expect_true("a" %in% unique(res$clusters$area))
#     expect_false("b" %in% unique(res$clusters$area))
#     expect_true(all(names(data1) == names(res)))
#     #compare attributes
#     for (t in names(attributes(data1))) {
#       if (class(attr(data1, t)) != "externalptr" & t != "type" & t != "opts"){
#         expect_true(all(attr(data1, t) == attr(res, t)),
#                     paste0("attr(data1, t) : ", attr(data1, t), " != ", "attr(res, t) : ", attr(res, t) ))
#       }
#     }
#
#     expect_true(all(class(data1) == class(res)))
#     expect_true(all(class(data1$areas) == class(res$areas)))
#   })
#
#   test_that("x and y can be antaresDataList but they must have the same names ", {
#     data1 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       links = getLinks(),
#       districts = getDistricts(),
#       clusters = getAreas(),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     data2 <- suppressWarnings(readAntares(
#       areas = c("a", "c"),
#       districts = getDistricts(),
#       clusters = getAreas(exclude = "b"),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     expect_error(antaresProcessing::compare(data1, data2))
#
#
#   })
#
#   #compare values data1, data2 et res
#
#   test_that("x and y can be antaresDataList, diff must work ", {
#     data1 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       links = getLinks(),
#       districts = getDistricts(),
#       clusters = getAreas(),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     data2 <- copy(data1)
#     data2$areas[, LOAD := LOAD * 1.2]
#     data2$links[link == "b - c", `FLOW LIN.` := `FLOW LIN.` + as.integer(10000)]
#     data2$clusters <- data2$clusters[area == "c" & cluster == "base", production := as.integer(production * 1.8)]
#     data2$districts <- data2$districts[, `MRG. PRICE` :=  `MRG. PRICE` + 1.20]
#     res <- antaresProcessing::compare(data1, data2)
#     #check
#     expect_equal(res$areas$LOAD, data1$areas$LOAD * 0.2)
#     expect_equal(res$links[link == "b - c", `FLOW LIN.`], rep(10000, 336))
#     expect_equal(res$clusters[area == "c" & cluster == "base", production ],  data1$clusters[area == "c" & cluster == "base", production ]*0.8)
#     expect_equal(res$districts[, `MRG. PRICE` ], rep(1.20, 336))
#   })
#
#   test_that("compare return a warning when diff is empty, no row", {
#     opts <- setSimulationPath(studyPath)
#     data1 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     data2 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       timeStep = "hourly",
#       showProgress = FALSE))
#     res <- antaresProcessing::compare(data1, data2)
#     expect_equal(dim(res)[1], 672)
#     data1 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       timeStep = "hourly",
#       showProgress = FALSE,
#       mcYears = 1))
#     data2 <- suppressWarnings(readAntares(
#       areas = c("a", "b"),
#       timeStep = "hourly",
#       showProgress = FALSE,
#       mcYears = 2))
#     expect_warning(antaresProcessing::compare(data1, data2))
#   })
#
# })
#
