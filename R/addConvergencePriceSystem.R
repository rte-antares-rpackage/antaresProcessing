#' addConvergencePriceSystem
#'
#' This function computes priceConvergenceSystem, priceConvergenceSystem represent
#' the biggest system without congestion.
#'
#' @param antaresData Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. antaresData must contains areas and
#'   links details data with linkCapacity.
#' @examples
#' \dontrun{
#'
#'   myData <- readAntares(areas = "all",
#'   links = "all",
#'   showProgress = FALSE,
#'   linkCapacity = TRUE,
#'   mcYears = "all")
#'
#'   myDataRV <- removeVirtualAreas(x = myData,
#'   storageFlexibility = getAreas(c("psp", "hub")),
#'   production = getAreas("off"))
#'
#'   addConvergencePriceSystem(myData)
#' }
#' @export
addConvergencePriceSystem <- function(antaresData = NULL){
  .check_x(antaresData)
  if((!is(antaresData, "antaresDataList")) | !("areas" %in% names(antaresData)) |
     !("links" %in% names(antaresData)) ){
    stop("Import areas and links data.")
  }
  if(attr(antaresData, "synthesis") | attr(antaresData, "timeStep") != "hourly"){
    stop("Import hourly details data")
  }

  if(is.null(antaresData$links$congestion)){
    antaresData <- addLoadFactorLink(antaresData)
  }
  antaresData$links[, ':=' (from = strsplit(link, split = " - " )[[1]][1],
                            to = strsplit(link, split = " - " )[[1]][2]),
                    by = .(link)]
  dontSeperate <- antaresData$links[congestion==0, .(mcYear, timeId, from, to)]

  converAreas <- dontSeperate[, list(allAreasTgList = list(c(from,to))) ,
                              by=.(mcYear, timeId)]

  converAreas[ , areasConver := as.vector(paste(sort(unique(allAreasTgList[[1]])),
                                                collapse = " ")),
               by=.(mcYear, timeId)]

  antaresData$areas[converAreas,(paste("priceConvergenceSystem")) := i.areasConver,
                    on=.(mcYear, timeId)]

  invisible(antaresData)
}
