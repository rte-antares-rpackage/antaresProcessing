# Copyright © 2016 RTE Réseau de transport d’électricité

#' compute termal generation from study
#' @param opts \code{simOptions} obtain wich \link[antaresRead]{setSimulationPath}
#' @export
termalGeneration <- function(opts = simOptions()){
  clDesc <- readClusterDesc(opts)
  clDesc <- clDesc[,list(termalGeneration = sum(unitcount*nominalcapacity)), by = c("area", "group")]
  clDesc
}
