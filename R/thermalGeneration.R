# Copyright © 2016 RTE Réseau de transport d’électricité

#' compute thermal capacities from study
#' @param opts \code{simOptions} obtain which \link[antaresRead]{setSimulationPath}
#' @export
thermalGroupCapacities <- function(opts = simOptions()){
  clDesc <- readClusterDesc(opts)
  clDesc <- clDesc[,list(thermalGroupCapacities = sum(unitcount*nominalcapacity)), by = c("area", "group")]
  clDesc
}
