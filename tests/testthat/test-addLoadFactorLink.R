context("add Load Factor Link")

#TO DO in this file

source("setup_test_case.R")
opts <- setSimulationPath(studyPath)

describe("addLoadFactorLink", {

  it("adds column 'LoadFactorLink' to the input data", {
    mydata <- suppressWarnings({readAntares(links="all", timeStep = "annual", showProgress = FALSE, linkCapacity=TRUE)})
    cols <- names(copy(mydata))
    addLoadFactorLink(mydata)
    expect_identical(c(cols, "loadFactorLink"), names(mydata))
  })

  it("column 'LoadFactorLink' must be numerical", {
    mydata <- suppressWarnings({readAntares(links="all", showProgress = FALSE, linkCapacity=TRUE)})
    addLoadFactorLink(mydata)
    expect_is(mydata$loadFactorLink, "integer")
  })

  it("column 'LoadFactorLink' must be positive", {
    mydata <- suppressWarnings({readAntares(links="all", showProgress = FALSE, linkCapacity=TRUE)})
    addLoadFactorLink(mydata)
    expect_gt(min(mydata$loadFactorLink), -1)
  })

  it("accepts 'antaresDataList' objects", {
    mydata <- suppressWarnings({readAntares(areas = "all", links="all", showProgress = FALSE, linkCapacity=TRUE)})
    addLoadFactorLink(mydata)

    expect_is(mydata, "antaresDataList")
    expect_false(is.null(mydata$links$loadFactorLink))
  })

  # it("column 'LoadFactorLink' must be smaller than capacity", {
  #   mydata <- suppressWarnings({readAntares(links="all", showProgress = FALSE, linkCapacity=TRUE)})
  #   addLoadFactorLink(mydata)
  #   #TO DO
  #   #on doit faire le test suivant uniquement si les liens ont une capacité mises à "enabled"
  #   #pour l'instant on ne répère pas les paramètres des liens comme pour les clusters
  #   # TO DO à rajouter dans antaresRead
  #   #       paramètres à récupérer dans les fichiers properties.ini
  #   expect_lt(mydata[`FLOW LIN.`>0,max(loadFactorLink-transCapacityDirect)],0)
  #   expect_lt(mydata[`FLOW LIN.`<0,max(loadFactorLink-transCapacityIndirect)],0)
  # })

})
