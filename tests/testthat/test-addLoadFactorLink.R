context("add Load Factor Link")

opts <- setSimulationPath(studyPath)

describe("addLoadFactorLink", {

  it("adds column 'loadFactor' and 'congestion' to the input data", {
    mydata <- suppressWarnings({readAntares(links="all", timeStep = "annual", showProgress = FALSE, linkCapacity=TRUE)})
    cols <- names(copy(mydata))
    addLoadFactorLink(mydata)
    expect_identical(c(cols, "loadFactor", "congestion"), names(mydata))
  })

  it("column 'loadFactor' and 'congestion' must be numerical", {
    mydata <- suppressWarnings({readAntares(links="all", showProgress = FALSE, linkCapacity=TRUE)})
    addLoadFactorLink(mydata)
    expect_is(mydata$loadFactor, "numeric")
    expect_is(mydata$congestion, "integer")
  })

  it("accepts 'antaresDataList' objects", {
    mydata <- suppressWarnings({readAntares(areas = "all", links="all", showProgress = FALSE, linkCapacity=TRUE)})
    addLoadFactorLink(mydata)

    expect_is(mydata, "antaresDataList")
    expect_false(is.null(mydata$links$loadFactor))
  })

  it( "error if links are missings and columns transCapacityDirect, transCapacityIndirect and FLOW LIN." , {

    mydata <- suppressWarnings({readAntares(areas = "all", showProgress = FALSE)})
    expect_error(addLoadFactorLink(mydata),"'x' does not contain link data")

    mydata <- suppressWarnings({readAntares(clusters = "all", showProgress = FALSE)})
    expect_error(addLoadFactorLink(mydata),"'x' does not contain link data")

    expect_error(addLoadFactorLink(c("areas", "links")),"'x' is not an 'antaresData' object")

  })

  it( "error if columns transCapacityDirect, transCapacityIndirect and FLOW LIN. are missing" , {

    mydata <- suppressWarnings({readAntares(areas = "all", links = "all", showProgress = FALSE)})
    expect_error(addLoadFactorLink(mydata),"The following columns are needed but missing: transCapacityDirect, transCapacityIndirect")

    mydata <- suppressWarnings({readAntares(areas = "all", links = "all", select = "production", showProgress = FALSE, linkCapacity = TRUE)})
    expect_error(addLoadFactorLink(mydata),"The following columns are needed but missing: FLOW LIN.")

  })

  # it("column 'LoadFactorLink' must be smaller than capacity", {
  #   mydata <- suppressWarnings({readAntares(links="all", showProgress = FALSE, linkCapacity=TRUE)})
  #   addLoadFactorLink(mydata)
  #   #TO DO
  #   #on doit faire le test suivant uniquement si les liens ont une capacité mises à "enabled"
  #   #pour l'instant on ne peut pas récupérer les paramètres des liens comme pour les clusters
  #   # TO DO à rajouter dans antaresRead
  #   #       paramètres à récupérer dans les fichiers properties.ini
  #   expect_lt(mydata[`FLOW LIN.`>0,max(loadFactorLink-transCapacityDirect)],0)
  #   expect_lt(mydata[`FLOW LIN.`<0,max(loadFactorLink-transCapacityIndirect)],0)
  # })


})
