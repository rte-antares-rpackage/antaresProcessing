# Copyright © 2016 RTE Réseau de transport d’électricité

#' compute thermal group capacities from study
#' @param opts \code{simOptions} obtain wich \link[antaresRead]{setSimulationPath}
#' @export
thermalGroupCapacities <- function(opts = simOptions()){
  clDesc <- readClusterDesc(opts)
  clDesc <- clDesc[,list(thermalGroupCapacity = sum(unitcount*nominalcapacity)), by = c("area", "group")]
  clDesc
}
