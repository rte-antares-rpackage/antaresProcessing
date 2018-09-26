#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import antaresRead
#' @importFrom methods is
#' @importFrom stats median quantile sd as.formula

.onLoad <- function(libname, pkgname){
  setAlias(
    "downwardMargin",
    "Data required by 'addDownwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "pumpingCapacity",
      "ROW BAL.", "linkCapacity", "mustRun")
  )

  setAlias(
    "upwardMargin",
    "Data required by 'addUpwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE", "pumpingCapacity", "storageCapacity",
      "ROW BAL.", "AVL DTG", "linkCapacity", "hydroStorageMaxPower")
  )

  setAlias(
    "exportsImports",
    "Data required by 'addExportAndImport()'",
    c("areas", "links", "FLOW LIN.", "linkCapacity")
  )

  setAlias(
    "loadFactorLink",
    "Data required by 'addLoadFactorLink()'",
    c("links", "FLOW LIN.", "linkCapacity")
  )

  setAlias(
    "externalDependency",
    "Data required by 'externalDependency()'",
    c("areas", "links", "LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND",
      "SOLAR", "AVL DTG", "FLOW LIN.", "pumpingCapacity", "storageCapacity", "mustRun", "linkCapacity", "hydroStorageMaxPower")
  )

  setAlias(
    "loadFactor",
    "Data required by 'loadFactor()'",
    c("clusters", "thermalModulation", "mcYears", "thermalAvailabilities")
  )

  setAlias(
    "modulation",
    "Data required by 'modulation()'",
    c("areas", "clusters", "mcYears", "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
      "MISC. DTG", "H. STOR", "H. ROR", "SOLAR", "WIND")
  )

  setAlias(
    "netLoadRamp",
    "Data required by function 'netLoadRamp()'",
    c("areas", "mcYears","LOAD", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND",
      "SOLAR", "BALANCE", "mustRun")
  )

  setAlias(
    "surplus",
    "Data required by function 'surplus()'",
    c("areas", "links", "mcYears", "LOAD", "MRG. PRICE", "OP. COST",
      "CONG. FEE (ALG.)", "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
      "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "PSP",
      "ROW BAL.", "HURDLE COST")
  )

  setAlias(
    "surplusClusters",
    "Data required by 'surplusClusters()'",
    c("areas", "clusters", "mcYears", "MRG. PRICE")
  )

  setAlias(
    "surplusSectors",
    "Data required by function 'surplusSectors()'",
    c("areas", "clusters", "mcYears", "WIND", "SOLAR", "H. ROR", "H. STOR",
      "MRG. PRICE")
  )
  sapply(names(pkgEnv$process), function(X){
    tpAlias <- pkgEnv$process[[X]]
    X <- paste0("Out_", X)
    sapply(names(tpAlias), function(Y){
      varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
    })
  })
}

globalVariables(
  c(
    ".", "AVL DTG", "BALANCE", "CONG. FEE (ALG.)", "FLOW LIN.", "H. ROR", "LOAD",
    "MISC. NDG", "MRG. PRICE", "NODU", "OV. COST", "PSP", "ROW BAL.", "SOLAR",
    "WIND", "absoluteModulation", "area", "areaRamp", "availableUnits",
    "balanceRamp", "cluster", "congestionFees", "consumerSurplus", "direction",
    "district", "downwardModulation", "economicGradient", "exportsLevel",
    "fixed.cost", "fixedCost", "from", "globalSurplus", "group", "hstorPMaxAvg",
    "importsLevel", "isolatedDownwardMargin", "isolatedUpwardMargin", "link",
    "marginal.cost", "max_absoluteModulation", "max_downwardModulation",
    "max_upwardModulation", "mcYear", "min.stable.power",
    "minGenModulation", "mustRunTotal", "netLoad", "nominalcapacity", "opCost",
    "prodLastUnit", "producerSurplus", "production", "pumpingCapacity",
    "quantile", "rowBalanceSurplus", "shiftProd", "spinning",
    "startup.cost", "startupCost", "storageCapacity", "storageSurplus",
    "surplusPerUnit", "thermalPmin", "timeId", "to", "toDistrict",
    "totalSurplus", "transCapacityDirect", "transCapacityIndirect",
    "unitcount", "upwardModulation", "variableCost",
    "interconnectedDownwardMargin", "interconnectedUpwardMargin",
    "thermalAvailability", "OP. COST"
  )
)

.idCols <- antaresRead:::.idCols
.addClassAndAttributes <- antaresRead:::.addClassAndAttributes
.groupByDistrict <- antaresRead:::.groupByDistrict

pkgEnv <- antaresRead:::pkgEnv

#-----------------------------  HDF5 ------------------------------------#

.requireRhdf5_Antares <- antaresRead:::.requireRhdf5_Antares
# Process H5

pkgEnv$process$addNetLoad$areas <- c("netLoad")
pkgEnv$process$addNetLoad$districts <- c("netLoad")


pkgEnv$process$addDownwardMargin$areas <- c("isolatedDownwardMargin",
                                                      "interconnectedDownwardMargin")

pkgEnv$process$addDownwardMargin$districts <- c("isolatedDownwardMargin",
                                            "interconnectedDownwardMargin")

pkgEnv$process$addUpwardMargin$areas <- c("isolatedUpwardMargin",
                                                    "interconnectedUpwardMargin")
pkgEnv$process$addUpwardMargin$districts <- c("isolatedUpwardMargin",
                                          "interconnectedUpwardMargin")

pkgEnv$process$addExportAndImport$areas <- c("import",
                                              "export",
                                             "capExport",
                                             "capImport")
pkgEnv$process$addExportAndImport$districts <- c("import",
                                             "export",
                                             "capExport",
                                             "capImport")

pkgEnv$process$addLoadFactorLink$links <- c("loadFactor",
                                                      "congestion")

pkgEnv$process$externalDependency$areas <- c("netLoad",
                                                       "exportsLevel",
                                                       "importsLevel",
                                                       "exportsFrequency",
                                                       "importsFrequency")

pkgEnv$process$externalDependency$districts <- c("netLoad",
                                             "exportsLevel",
                                             "importsLevel",
                                             "exportsFrequency",
                                             "importsFrequency")



pkgEnv$process$loadFactor$clusters <- c("loadFactor", "propHoursMinGen", "propHoursMaxGen", "loadFactorAvailable")

pkgEnv$process$modulation$clusters <- c("upwardModulation", "downwardModulation",
                                                  "absoluteModulation")

pkgEnv$process$netLoadRamp$areas <- c("netLoadRamp", "balanceRamp", "areaRamp")
pkgEnv$process$netLoadRamp$districts <- c("netLoadRamp", "balanceRamp", "areaRamp")

pkgEnv$process$surplus$areas <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                            "storageSurplus", "congestionFees", "globalSurplus")

pkgEnv$process$surplus$districts <- c("consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                  "storageSurplus", "congestionFees", "globalSurplus")


pkgEnv$process$surplusClusters$clusters <- c("variableCost", "fixedCost", "startupCost",
                                                       "surplusPerUnit", "totalSurplus", "economicGradient", "surplusLastUnit")





#pkgEnv$process$surplusSectors$areas <- c("surplus", "cost")

pkgEnv$processDispo <- data.frame(
  trtName = c(
              "netLoad",
              "downwardMargin",
              "upwardMargin",
              "exportsImports",
              "loadFactorLink",
              "externalDependency",
              "loadFactor",
              "modulation",
              "netLoadRamp",
              "surplus",
              "surplusClusters"
  )
  , fctname = c(
                 "addNetLoad",
                 "addDownwardMargin" ,
                 "addUpwardMargin" ,
                 "addExportAndImport" ,
                 "addLoadFactorLink" ,
                 "externalDependency" ,
                 "loadFactor" ,
                 "modulation" ,
                 "netLoadRamp" ,
                 "surplus" ,
                 "surplusClusters"
  ))

