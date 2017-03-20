context("addDownwardMargin")

opts <- setSimulationPath(studyPath)
data <- suppressWarnings(readAntares(
  "all", "all",
  select = c("H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE",
             "ROW BAL.", "FLOW LIN."),
  linkCapacity = TRUE, mustRun = TRUE,
  showProgress = FALSE
))
dataCor <- removeVirtualAreas(data, getAreas("psp"))

describe("addDownwardMargin", {
  it("adds two columns with isolated and interconnected downward margins", {
    it("with virtual areas", {
      data <- data.table::copy(dataCor)
      expect_silent(addDownwardMargin(data))
      expect_false(is.null(data$areas$isolatedDownwardMargin))
      expect_false(is.null(data$areas$interconnectedDownwardMargin))
      expect_equal(data$areas[, thermalPmin + `H. ROR`+WIND+SOLAR+`MISC. NDG` - pumpingCapacity - LOAD],
                   data$areas$isolatedDownwardMargin)
      expect_equal(data$areas[, isolatedDownwardMargin + BALANCE - `ROW BAL.`],
                   data$areas$interconnectedDownwardMargin)
    })
    it("without virtual areas", {
      data <- data.table::copy(data)
      expect_silent(addDownwardMargin(data))
      expect_false(is.null(data$areas$isolatedDownwardMargin))
      expect_false(is.null(data$areas$interconnectedDownwardMargin))
      expect_equal(data$areas[, thermalPmin + `H. ROR`+WIND+SOLAR+`MISC. NDG`-LOAD],
                   data$areas$isolatedDownwardMargin)
      expect_equal(data$areas[, isolatedDownwardMargin + BALANCE - `ROW BAL.`],
                   data$areas$interconnectedDownwardMargin)
    })
  })

  it ("throws an error if a required column is missing", {
    data2 <- data.table::copy(data)
    data2$areas$SOLAR <- NULL
    expect_error(addDownwardMargin(data2), "SOLAR")
    data2 <- data.table::copy(data)
    data2$areas$thermalPmin <- NULL
    expect_error(addDownwardMargin(data2), "mustRun = TRUE")
  })

  describe("missing pumpingCapacity", {
    it ("throws an error if there are virtual storage/flexibility areas", {
      data <- data.table::copy(dataCor)
      data$areas$pumpingCapacity <- NULL
      expect_error(addDownwardMargin(data), "linkCapacity = TRUE")
    })
    it ("works silently if not", {
      data <- data.table::copy(data)
      expect_silent(addDownwardMargin(data))
    })
  })

  it("also works with antaresDataTable objects", {
    data <- data.table::copy(data$areas)
    expect_silent(addDownwardMargin(data))
    expect_false(is.null(data$isolatedDownwardMargin))
    expect_false(is.null(data$interconnectedDownwardMargin))
  })

  it("does not work with other objects", {
    expect_error(addDownwardMargin(TRUE), "antaresData")
  })
})
