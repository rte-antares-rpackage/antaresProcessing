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
    expect_gt(min(s$hstorPMaxAvg), -1)

    s <- margins(districtsOnly)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$hstorPMaxAvg), -1)
  })

  it("check formula", {
    s <- margins(areasOnly)
    expect_equal(s[, `AVL DTG`]+mydata$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydata$areas[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(areasOnly, type="downward")
    expect_equal(s[, thermalPmin]+areasOnly$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnly$areas[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(areasOnly, type="both")
    expect_equal(s[, `AVL DTG`]+areasOnly$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-areasOnly$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin]+areasOnly$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnly$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(districtsOnly)
    expect_equal(s[, `AVL DTG`]+mydata$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydata$districts[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(districtsOnly, type="downward")
    expect_equal(s[, thermalPmin]+districtsOnly$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnly$districts[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(districtsOnly, type="both")
    expect_equal(s[, `AVL DTG`]+districtsOnly$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-districtsOnly$districts[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin]+districtsOnly$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnly$districts[, BALANCE], s$interconnectedDownwardMargin)

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
    expect_gt(min(s$hstorPMaxAvg), -1)

    s <- margins(districtsOnlyH)
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$hstorPMaxAvg), -1)
  })

  it("hourly : check formula", {
    s <- margins(areasOnlyH)
    expect_equal(s[, `AVL DTG`]+areasOnlyH$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-areasOnlyH$areas[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(areasOnlyH, type="downward")
    expect_equal(s[, thermalPmin]+areasOnlyH$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnlyH$areas[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(areasOnlyH, type="both")
    expect_equal(s[, `AVL DTG`]+areasOnlyH$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-areasOnlyH$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin]+areasOnlyH$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnlyH$areas[, BALANCE], s$interconnectedDownwardMargin)

    expect_length(grep(TRUE, is.na(s$isolatedDownwardMargin)), 0)
    expect_length(grep(TRUE, is.na(s$interconnectedUpwardMargin)), 0)

    s <- margins(districtsOnlyH)
    expect_equal(s[, `AVL DTG`]+districtsOnlyH$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-districtsOnlyH$districts[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(districtsOnlyH, type="downward")
    expect_equal(s[, thermalPmin]+districtsOnlyH$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnlyH$districts[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(districtsOnlyH, type="both")
    expect_equal(s[, `AVL DTG`]+districtsOnlyH$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-districtsOnlyH$districts[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin]+districtsOnlyH$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnlyH$districts[, BALANCE], s$interconnectedDownwardMargin)

    expect_length(grep(TRUE, is.na(s$isolatedDownwardMargin)), 0)
    expect_length(grep(TRUE, is.na(s$interconnectedUpwardMargin)), 0)
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
    expect_gt(min(s$hstorPMaxAvg), -1)
    expect_gt(min(s$storageCapacity), -1)

    s <- margins(mydataRVA)
    s<-s$districts
    expect_gt(min(s$`AVL DTG`), -1)
    expect_gt(min(s$hstorPMaxAvg), -1)
    expect_gt(min(s$storageCapacity), -1)
  })

  it("RVA : hourly : check formula", {
    s <- margins(areasOnlyHRVA)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$areas[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(areasOnlyHRVA, type="downward")
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$areas[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(areasOnlyHRVA, type="both")
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(mydataRVA)
    s<-s$districts
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$districts[, BALANCE], s$interconnectedUpwardMargin)

    s <- margins(mydataRVA, type="downward")
    s<-s$districts
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$districts[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(mydataRVA, type="both")
    s<-s$districts
    expect_equal(s[, thermalPmin-pumpingCapacity]+mydataRVA$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+mydataRVA$districts[, BALANCE], s$interconnectedDownwardMargin)
    expect_equal(s[, `AVL DTG`+storageCapacity]+mydataRVA$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-mydataRVA$districts[, BALANCE], s$interconnectedUpwardMargin)

  })


  mydataM <- suppressWarnings({
    readAntares(areas = "all", clusters = "all", districts = "all",
                thermalAvailabilities = TRUE,
                hydroStorageMaxPower = TRUE,
                showProgress = FALSE)
  })

  areasOnlyM <- mydataM
  areasOnlyM$districts <- NULL

  districtsOnlyM <- mydataM
  districtsOnlyM$areas <- NULL

  it("mustRun : hourly : check formula when mustRun is not needed", {
    s <- margins(areasOnlyM, ignoreMustRun = TRUE)
    expect_equal(s[, `AVL DTG`]+areasOnlyM$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-areasOnlyM$areas[, BALANCE], s$interconnectedUpwardMargin)

    s<-margins(areasOnlyM, type="downward", ignoreMustRun = TRUE)
    expect_equal(s[, thermalPmin]+areasOnlyM$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnlyM$areas[, BALANCE], s$interconnectedDownwardMargin)

    s<-margins(areasOnlyM, type="both", ignoreMustRun = TRUE)
    expect_equal(s[, `AVL DTG`]+areasOnlyM$areas[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-areasOnlyM$areas[, BALANCE], s$interconnectedUpwardMargin)
    expect_equal(s[, thermalPmin]+areasOnlyM$areas[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+areasOnlyM$areas[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(districtsOnlyM, ignoreMustRun = TRUE)
    expect_equal(s[, `AVL DTG`]+districtsOnlyM$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-districtsOnlyM$districts[, BALANCE], s$interconnectedUpwardMargin)

    s <- margins(districtsOnlyM, type="downward", ignoreMustRun = TRUE)
    expect_equal(s[, thermalPmin]+districtsOnlyM$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnlyM$districts[, BALANCE], s$interconnectedDownwardMargin)

    s <- margins(districtsOnlyM, type="both", ignoreMustRun = TRUE)
    expect_equal(s[, thermalPmin]+districtsOnlyM$districts[, `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD], s$isolatedDownwardMargin)
    expect_equal(s[, isolatedDownwardMargin]+districtsOnlyM$districts[, BALANCE], s$interconnectedDownwardMargin)
    expect_equal(s[, `AVL DTG`]+districtsOnlyM$districts[, WIND+SOLAR+`H. ROR`+`MISC. NDG`+hstorPMaxAvg-LOAD], s$isolatedUpwardMargin)
    expect_equal(s[, isolatedUpwardMargin]-districtsOnlyM$districts[, BALANCE], s$interconnectedUpwardMargin)

  })


})

