# Copyright © 2016 RTE Réseau de transport d’électricité

#' compute thermal capacities from study
#' @param opts \code{simOptions} obtain which \link[antaresRead]{setSimulationPath}
#' @return A data.table, data.frame class object containing computed result by areas/clusters
#'
#' @examples
#' \donttest{
#' library(antaresRead)
#' # with study test for example (study is in package antaresRead)
#' sourcedir <- system.file("testdata", package = "antaresRead")
#'
#' # untar study in temp dir
#' path_latest <- file.path(tempdir(), "latest")
#' untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path_latest)
#'
#' study_path <- file.path(path_latest, "test_case")
#'
#' # set path to your Antares simulation
#' opts <- setSimulationPath(study_path)
#'
#' mydata <- readAntares( areas = "all",
#'                        mcYears = "all", showProgress = FALSE)
#'
#' # long to wide format
#' res <- thermalGroupCapacities(opts = opts)
#' }
#' @export
thermalGroupCapacities <- function(opts = simOptions()){
  clDesc <- readClusterDesc(opts)
  clDesc <- clDesc[,list(thermalGroupCapacities = sum(unitcount*nominalcapacity)), by = c("area", "group")]
  clDesc
}
