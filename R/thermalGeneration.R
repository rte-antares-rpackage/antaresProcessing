# Copyright © 2016 RTE Réseau de transport d’électricité

#' compute thermal generation from study
#' @param opts \code{simOptions} obtain wich \link[antaresRead]{setSimulationPath}
#' @export
thermalGeneration <- function(opts = simOptions()){
  clDesc <- readClusterDesc(opts)
  clDesc <- clDesc[,list(thermalGeneration = sum(unitcount*nominalcapacity)), by = c("area", "group")]
  clDesc
}
