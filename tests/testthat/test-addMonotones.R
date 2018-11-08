context("Function addMonotones")

opts <- setSimulationPath(studyPath)

test_that("addMonotones works with an antaresData detail", {
  expect_error(addMonotones(antaresData = 1e-10),
               regexp = NULL)

  myDataS <- readAntares(showProgress = FALSE)
  expect_error(addMonotones(antaresData = myDataS),
               regexp = NULL)

})

test_that("addMonotones work with a correct variable", {
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE)
  expect_error(addMonotones(antaresData = myDataD,
                            variable = 3),
               regexp = "variable is not a character")
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE)
  expect_error(addMonotones(antaresData = myDataD,
                            variable = 3),
               regexp = "variable is not a character")
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE, links = "all")
  expect_error(addMonotones(antaresData = myDataD,
                            variable = "LOAD"),
               regexp = "Incorrect variable")
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE, areas = "all")
  addMonotones(antaresData = myDataD,
                            variable = "LOAD")
})

test_that("addMonotones works with an antaresDataTable", {
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE, areas = "all")
  addMonotones(antaresData = myDataD,
               variable = "LOAD")
  expect_true("LOADMonoMc1" %in% names(myDataD))
  expect_true("LOADMonoMc2" %in% names(myDataD))
  expect_true("LOADMonoMean" %in% names(myDataD))
  expect_true(is.integer(myDataD$LOADMonoMc1))
  expect_true(is.integer(myDataD$LOADMonoMc2))
  expect_true(is.integer(myDataD$LOADMonoMean))
  myDataD <- readAntares(mcYears = "all", showProgress = FALSE, areas = "all")
  addMonotones(antaresData = myDataD,
               variable = "MRG. PRICE")
  expect_true("MRG. PRICEMonoMc1" %in% names(myDataD))
  expect_true("MRG. PRICEMonoMc2" %in% names(myDataD))
  expect_true("MRG. PRICEMonoMean" %in% names(myDataD))
  expect_true(is.numeric(myDataD$`MRG. PRICEMonoMc1`))
  expect_true(is.numeric(myDataD$`MRG. PRICEMonoMc2`))
  expect_true(is.numeric(myDataD$`MRG. PRICEMonoMean`))

  for(myArea in unique(myDataD$area)){
    priceAmc1 <- myDataD[area==myArea & mcYear==1, `MRG. PRICE` ]
    priceAmc1Mono <- sort(priceAmc1, decreasing = TRUE)
    expect_equal(myDataD[area==myArea & mcYear==1, `MRG. PRICEMonoMc1` ],
                 priceAmc1Mono)
  }
  for(myArea in unique(myDataD$area)){
    priceAmc1 <- myDataD[area==myArea & mcYear==2, `MRG. PRICE` ]
    priceAmc1Mono <- sort(priceAmc1, decreasing = TRUE)
    expect_equal(myDataD[area==myArea & mcYear==2, `MRG. PRICEMonoMc2` ],
                 priceAmc1Mono)
  }

})

test_that("addMonotones works with an antaresDataList", {
  myDataD <- readAntares(mcYears = "all",
                         showProgress = FALSE,
                         areas = "all",
                         links = "all")
  addMonotones(antaresData = myDataD,
               variable = "LOAD")
  expect_true("LOADMonoMc1" %in% names(myDataD$areas))
  expect_true("LOADMonoMc2" %in% names(myDataD$areas))
  expect_true("LOADMonoMean" %in% names(myDataD$areas))
  expect_true(is.integer(myDataD$areas$LOADMonoMc1))
  expect_true(is.integer(myDataD$areas$LOADMonoMc2))
  expect_true(is.integer(myDataD$areas$LOADMonoMean))
  myDataD <- readAntares(mcYears = "all",
                         showProgress = FALSE,
                         areas = "all",
                         links = "all")
  addMonotones(antaresData = myDataD,
               variable = "FLOW LIN.")
  expect_true("FLOW LIN.MonoMc1" %in% names(myDataD$links))
  expect_true("FLOW LIN.MonoMc2" %in% names(myDataD$links))
  expect_true("FLOW LIN.MonoMean" %in% names(myDataD$links))
  expect_true(is.numeric(myDataD$links$`FLOW LIN.MonoMc1`))
  expect_true(is.numeric(myDataD$links$`FLOW LIN.MonoMc2`))
  expect_true(is.numeric(myDataD$links$`FLOW LIN.MonoMean`))

  for(myLink in unique(myDataD$links$link)){
    flowLMc1 <- myDataD$links[link==myLink & mcYear==1, `FLOW LIN.` ]
    flowLMc1Mono <- sort(flowLMc1, decreasing = TRUE)
    expect_equal(myDataD$links[link==myLink & mcYear==1, `FLOW LIN.MonoMc1` ],
                 flowLMc1Mono)
  }
  for(myLink in unique(myDataD$links$link)){
    flowLMc2 <- myDataD$links[link==myLink & mcYear==2, `FLOW LIN.` ]
    flowLMc2Mono <- sort(flowLMc2, decreasing = TRUE)
    expect_equal(myDataD$links[link==myLink & mcYear==2, `FLOW LIN.MonoMc2` ],
                 flowLMc2Mono)
  }

  myDataD <- readAntares(mcYears = "all",
                         showProgress = FALSE,
                         areas = "all",
                         districts = "all")
  addMonotones(antaresData = myDataD,
               variable = "LOAD")
  expect_true("LOADMonoMc1" %in% names(myDataD$areas))
  expect_true("LOADMonoMc2" %in% names(myDataD$areas))
  expect_true("LOADMonoMean" %in% names(myDataD$areas))
  expect_true("LOADMonoMc1" %in% names(myDataD$districts))
  expect_true("LOADMonoMc2" %in% names(myDataD$districts))
  expect_true("LOADMonoMean" %in% names(myDataD$districts))
})

test_that("addMonotones computes the same monotones for several years", {
  myDataD <- readAntares(mcYears = "all",
                         showProgress = FALSE,
                         areas = "all")
  addMonotones(antaresData = myDataD,
               variable = "LOAD")
  expect_true("LOADMonoMc1" %in% names(myDataD))
  expect_true("LOADMonoMc2" %in% names(myDataD))
  expect_true("LOADMonoMean" %in% names(myDataD))
  expect_equal(myDataD[mcYear==1, LOADMonoMc1],
               myDataD[mcYear==2, LOADMonoMc1])
  expect_equal(myDataD[mcYear==1, LOADMonoMc2],
               myDataD[mcYear==2, LOADMonoMc2])
  expect_equal(myDataD[mcYear==1, LOADMonoMean],
               myDataD[mcYear==2, LOADMonoMean])
})
