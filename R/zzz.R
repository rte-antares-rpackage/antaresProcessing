#Copyright © 2016 RTE Réseau de transport d’électricité

#' @import data.table
#' @import antaresRead
#' @importFrom methods is
#' @importFrom stats median quantile sd as.formula

.onLoad <- function(libname, pkgname){
  setAlias(
    "downwardMargin",
    "Data required by 'addDownwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE",
      "ROW BAL.", "linkCapacity", "mustRun")
  )

  setAlias(
    "upwardMargin",
    "Data required by 'addUpwardMargin()'",
    c("areas", "links", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "LOAD", "BALANCE",
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
      "SOLAR", "AVL DTG", "FLOW LIN.", "mustRun", "linkCapacity", "hydroStorageMaxPower")
  )

  setAlias(
    "loadFactor",
    "Data required by 'loadFactor()'",
    c("clusters", "thermalModulation", "mcYears")
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
    c("areas", "links", "mcYears", "LOAD", "MRG. PRICE", "OV. COST",
      "CONG. FEE (ALG.)", "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
      "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR", "MISC. NDG", "PSP",
      "ROW BAL.")
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
    "interconnectedDownwardMargin", "interconnectedUpwardMargin"
  )
)

.idCols <- antaresRead:::.idCols
.addClassAndAttributes <- antaresRead:::.addClassAndAttributes
.groupByDistrict <- antaresRead:::.groupByDistrict
pkgEnv <- antaresRead:::pkgEnv
