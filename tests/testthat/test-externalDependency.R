context("Function externalDependency")


sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)

  describe("externalDependency", {

    it("stops if time step is not hourly", {
      data2 <- suppressWarnings(readAntares(areas="all", timeStep = "annual",
                                            showProgress = FALSE, mcYears = "all", hydroStorageMaxPower = TRUE))
      addNetLoad(data2, ignoreMustRun = TRUE)
      expect_error(externalDependency(data2), "hourly")
    })

    it("stops if frequency exceeds a level ", {
      data2 <- suppressWarnings(readAntares(areas="all", showProgress = FALSE, hydroStorageMaxPower = TRUE))
      addNetLoad(data2, ignoreMustRun = TRUE)
      resH<-suppressWarnings(externalDependency(data2, timeStep = "hourly"))
      expect_lte(max(resH$exportsFrequency), 1)

      resW<-suppressWarnings(externalDependency(data2, timeStep = "weekly"))
      expect_lte(max(resW$exportsFrequency), 168)

      resM<-suppressWarnings(externalDependency(data2, timeStep = "monthly"))
      expect_lte(max(resM$exportsFrequency), 744)

      resA<-suppressWarnings(externalDependency(data2, timeStep = "annual"))
      expect_lte(max(resA$exportsFrequency), 8736)
    })

    it("accepts 'antaresDataList' objects : areas and links", {
      mydata <- suppressWarnings({readAntares(areas = "all", links="all", showProgress = FALSE, hydroStorageMaxPower = TRUE)})

      expect_is(mydata, "antaresDataList")

      mydata<-addNetLoad(mydata, ignoreMustRun = TRUE)
      res<-externalDependency(mydata, timeStep = "hourly")
      expect_false(is.null(res$exportsLevel))
    })

    it("accepts 'antaresDataList' objects : areas and districts", {
      mydata <- suppressWarnings({readAntares(areas = "all", districts ="all", showProgress = FALSE, hydroStorageMaxPower = TRUE)})

      expect_is(mydata, "antaresDataList")

      mydata<-addNetLoad(mydata, ignoreMustRun = TRUE)
      res<-externalDependency(mydata, timeStep = "hourly")
      expect_false(is.null(res$areas$exportsFrequency))
      expect_false(is.null(res$districts$exportsLevel))
    })

    it("accepts 'antaresDataList' objects : areas, districts and links", {
      mydata <- suppressWarnings({readAntares(areas = "all", districts ="all", links = "all", showProgress = FALSE, hydroStorageMaxPower = TRUE)})

      expect_is(mydata, "antaresDataList")

      mydata<-addNetLoad(mydata, ignoreMustRun = TRUE)
      res<-externalDependency(mydata, timeStep = "hourly")
      expect_true(is.null(res$links))
      expect_false(is.null(res$areas$exportsFrequency))
      expect_false(is.null(res$districts$exportsLevel))
    })

    it("works with virtual nodes", {
      mydata <- suppressWarnings({readAntares(areas = "all",
                                              districts ="all",
                                              links = "all",
                                              showProgress = FALSE,
                                              hydroStorageMaxPower = TRUE,
                                              linkCapacity = TRUE,
                                              mcYears = 1)})
      attrMyData<-attributes(mydata)
      expect_true(is.null(attrMyData$virtualNodes))

      mydataCorrected <- removeVirtualAreas(mydata,
                                            storageFlexibility = c(getAreas("psp"),
                                                                   getAreas("hub")))
      attrMyDataCorrected <- attributes(mydataCorrected)
      expect_false(is.null(attrMyDataCorrected$virtualNodes))

      mydataCorrected <- addNetLoad(mydataCorrected, ignoreMustRun = TRUE)
      res <- externalDependency(mydataCorrected, timeStep = "hourly")
      expect_true(is.null(res$links))
      expect_false(is.null(res$areas$exportsFrequency))
      expect_false(is.null(res$districts$exportsLevel))
    })

  })

})

