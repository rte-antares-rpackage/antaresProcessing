context("Function .fillClusterDesc")

opts <- setSimulationPath(studyPath)

clusterDesc <- readClusterDesc()

describe(".fillClusterDesc", {

  it("fill missing values with provided defaults", {
    .fillClusterDesc(clusterDesc, marginal.cost = 0, min.stable.power = 0)
    expect_false(any(is.na(clusterDesc$marginal.cost)))
    expect_false(any(is.na(clusterDesc$min.stable.power)))
  })

  it("creates columns if they are not present", {
    .fillClusterDesc(clusterDesc, fictiveCol = "ok !")
    expect_false(is.null(clusterDesc$fictiveCol))
    expect_true(all(clusterDesc$fictiveCol == "ok !"))
  })

})
