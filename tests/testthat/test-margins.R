context("Function margins")

opts <- setSimulationPath(studyPath)

describe("margins", {
  mydata <- suppressWarnings({
    readAntares(areas = "all", clusters = "all", districts = "all",
                mustRun = TRUE,
                thermalAvailabilities = TRUE,
                hydroStorageMaxPower = TRUE,
                showProgress = FALSE, timeStep = "annual")
    })

  areasOnly <- mydata
  areasOnly$districts <- NULL

  districtsOnly <- mydata
  districtsOnly$areas <- NULL

  it("returns an antaresDataTable if there is only areas or districts in 'x'", {
    s <- margins(areasOnly)
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s), length(unique(areasOnly$areas$area)))

    s <- margins(districtsOnly)
    expect_is(s, "antaresDataTable")
    expect_equal(nrow(s), length(unique(districtsOnly$districts$district)))
  })

  it("It returns an antaresDataList if there is area and district data in 'x'", {
    s <- margins(mydata)
    expect_is(s, "antaresDataList")
  })

  it("column 'thermalPmin' and 'AVL DTG' must be positive and others", {
    s <- margins(areasOnly)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)


    s <- margins(districtsOnly)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)
  })

  it("check formula", {
    s <- margins(areasOnly)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydata$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydata$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydata$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydata$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(districtsOnly)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydata$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydata$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydata$districts[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydata$districts[, BALANCE], s$interconnectedDownwardMargin)
  })

  mydataH <- suppressWarnings({
    readAntares(areas = "all", clusters = "all", districts = "all",links = "all",
                mustRun = TRUE,
                thermalAvailabilities = TRUE,
                hydroStorageMaxPower = TRUE,
                showProgress = FALSE, linkCapacity = TRUE)
  })

  areasOnlyH <- mydataH
  areasOnlyH$districts <- NULL

  districtsOnlyH <- mydataH
  districtsOnlyH$areas <- NULL

  it("hourly : returns an antaresDataTable if there is only areas or districts in 'x'", {
    s <- margins(areasOnlyH)
    expect_is(s, "antaresDataTable")
    expect_equal(length(unique(s$area)), length(unique(areasOnlyH$areas$area)))

    s <- margins(districtsOnlyH)
    expect_is(s, "antaresDataTable")
    expect_equal(length(unique(s$district)), length(unique(districtsOnlyH$districts$district)))
  })

  it("hourly : It returns an antaresDataList if there is area and district data in 'x'", {
    s <- margins(mydataH)
    expect_is(s, "antaresDataList")
  })

  it("hourly : column 'thermalPmin' and 'AVL DTG' must be positive and others", {
    s <- margins(areasOnlyH)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)


    s <- margins(districtsOnlyH)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)
  })

  it("hourly : check formula", {
    s <- margins(areasOnlyH)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataH$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataH$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataH$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataH$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(districtsOnlyH)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataH$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataH$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataH$districts[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataH$districts[, BALANCE], s$interconnectedDownwardMargin)
  })

  mydataRVA<-removeVirtualAreas(mydataH, storageFlexibility = getAreas(select = c("psp", "hub")))

  areasOnlyHRVA <- mydataRVA
  areasOnlyHRVA$districts <- NULL

  districtsOnlyHRVA <- mydataRVA
  districtsOnlyHRVA$areas <- NULL

  it("RVA : hourly : returns an antaresDataTable if there is only areas or districts in 'x'", {
    s <- margins(areasOnlyHRVA)
    expect_is(s, "antaresDataTable")
    expect_equal(length(unique(s$area)), length(unique(areasOnlyHRVA$areas$area)))

    expect_error(margins(districtsOnlyHRVA), "when there is virtual areas 'x' has to contain 'area'")
  })

  it("RVA : hourly : It returns an antaresDataList if there is area and district data in 'x'", {
    s <- margins(mydataRVA)
    expect_is(s, "antaresDataList")
    expect_equal(length(unique(s$districts$district)), length(unique(mydataRVA$districts$district)))
  })

  it("RVA : hourly : column 'thermalPmin' and 'AVL DTG' must be positive and others", {
    s <- margins(areasOnlyHRVA)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)

    s <- margins(mydataRVA)
    s<-s$districts
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$thermalPmin), -1)
    expect_gt(min(s$pumpingCapacity), -1)
    expect_gt(min(s$storageCapacity), -1)
  })

  it("RVA : hourly : check formula", {
    s <- margins(areasOnlyHRVA)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(mydataRVA)
    s<-s$districts
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$districts[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$districts[, BALANCE], s$interconnectedDownwardMargin)
  })
})

