context("addUpwardMargin")

sapply(studyPathS, function(studyPath){

  opts <- setSimulationPath(studyPath)
  data <- suppressWarnings(readAntares(
    "all", "all",
    select = c("H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "AVL DTG",
               "ROW BAL.", "FLOW LIN."),
    hydroStorageMaxPower = TRUE, linkCapacity = TRUE,
    showProgress = FALSE
  ))
  dataCor <- suppressWarnings(removeVirtualAreas(data, getAreas(c("psp", "hub"))))

  describe("addUpwardMargin", {
    it ("adds two columns with isolated and connected upward margins", {
      it ("with virtual areas", {
        data <- data.table::copy(dataCor)
        expect_silent(addUpwardMargin(data))
        expect_false(is.null(data$areas$isolatedUpwardMargin))
        expect_false(is.null(data$areas$interconnectedUpwardMargin))
        if (attr(data, "opts")$antaresVersion < 650) {
          expect_equal(data$areas[, `AVL DTG`+storageCapacity + WIND + SOLAR + `H. ROR` + `MISC. NDG` + hstorPMaxAvg - LOAD],
                       data$areas$isolatedUpwardMargin)
        } else {
          expect_equal(data$areas[, `AVL DTG`+storageCapacity + WIND + SOLAR + `H. ROR` + `MISC. NDG` + generatingMaxPower - LOAD],
                       data$areas$isolatedUpwardMargin)
        }
        expect_equal(data$areas[, isolatedUpwardMargin - BALANCE + `ROW BAL.`],
                     data$areas$interconnectedUpwardMargin)
      })

      it("without virtual areas", {
        data <- data.table::copy(data)
        expect_silent(addUpwardMargin(data))
        expect_false(is.null(data$areas$isolatedUpwardMargin))
        expect_false(is.null(data$areas$interconnectedUpwardMargin))
        if (attr(data, "opts")$antaresVersion < 650) {
          expect_equal(data$areas[, `AVL DTG` + WIND + SOLAR + `H. ROR` + `MISC. NDG` + hstorPMaxAvg - LOAD],
                       data$areas$isolatedUpwardMargin)
        } else {
          expect_equal(data$areas[, `AVL DTG` + WIND + SOLAR + `H. ROR` + `MISC. NDG` + generatingMaxPower - LOAD],
                       data$areas$isolatedUpwardMargin)
        }
        expect_equal(data$areas[, isolatedUpwardMargin - BALANCE + `ROW BAL.`],
                     data$areas$interconnectedUpwardMargin)
      })
    })

    if (attr(data, "opts")$antaresVersion < 650) {
      it ("throws a warning if hstorPMaxAvg is missing", {
        data <- data.table::copy(data)
        data$areas[, hstorPMaxAvg := NULL]
        expect_warning(addUpwardMargin(data), "hydroStorageMaxPower")
      })
    }

    it ("throws an error if a required column is missing", {
      data <- data.table::copy(data)
      data$areas[, LOAD := NULL]
      expect_error(addUpwardMargin(data), "LOAD")
    })

    describe("missing storageCapacity", {
      it ("throws error if there are pumped storage areas", {
        data <- data.table::copy(dataCor)
        data$areas[, storageCapacity := NULL]
        expect_error(addUpwardMargin(data), "storageCapacity")
      })
      it ("works silently when there is no pumped storage areas", {
        data <- data.table::copy(data)
        expect_silent(addUpwardMargin(data))
      })
    })

    it ("also works with antaresDataTable objects", {
      data <- data.table::copy(dataCor$areas)
      expect_silent(addUpwardMargin(data))
      expect_false(is.null(data$isolatedUpwardMargin))
      expect_false(is.null(data$interconnectedUpwardMargin))
      if (attr(data, "opts")$antaresVersion < 650) {
        expect_equal(data[, `AVL DTG` + storageCapacity + WIND + SOLAR + `H. ROR` + `MISC. NDG` + hstorPMaxAvg - LOAD],
                     data$isolatedUpwardMargin)
      } else {
        expect_equal(data[, `AVL DTG` + storageCapacity + WIND + SOLAR + `H. ROR` + `MISC. NDG` + generatingMaxPower - LOAD],
                     data$isolatedUpwardMargin)
      }
      expect_equal(data[, isolatedUpwardMargin - BALANCE + `ROW BAL.`],
                   data$interconnectedUpwardMargin)
    })

    it ("does not work with other objects", {
      expect_error(addUpwardMargin(TRUE), "antaresData")
    })

    data <- readAntares(links = "all", clusters = "all", showProgress = FALSE)
    it("warning no area no district", {
      expect_warning(addUpwardMargin(data), "'x' does not contain area or district data. Upward margin has not been computed.")
    })

  })

})
