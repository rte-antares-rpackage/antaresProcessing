#' addConvergencePriceArea
#'
#' This function computes priceConvergenceArea, priceConvergenceArea represent
#' the biggest system without congestion for one area.
#'
#' @param antaresData Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. antaresData must contains areas and
#'   links details hourly data with linkCapacity.
#' @export
addConvergencePriceArea <- function(antaresData = NULL){
  .check_x(antaresData)
  if((!is(antaresData, "antaresDataList")) | !("areas" %in% names(antaresData))){
    stop("Import areas data. antaresData must be an antaresDataList.")
  }
  if(attr(antaresData, "synthesis") | attr(antaresData, "timeStep") != "hourly"){
    stop("Import hourly details data")
  }

  if(is.null(antaresData$areas$neighbours)){
      addNeighbours(antaresData)
  }
  if(is.null(antaresData$areas$priceConvergenceSystem)){
    addConvergencePriceSystem(antaresData)
  }
  #priceConvergenceArea = area
  antaresData$areas[, priceConvergenceAreaN := as.character(area),
                    by=.(area, mcYear, timeId)]
  antaresData$areas[, ':=' (neighboursN = neighbours)]

  #we must get the virtual areas
  resAttr <- attributes(antaresData)
  virtualAreas <- resAttr$virtualNodes$storageFlexibility
  virtualAreas <- c(virtualAreas, resAttr$virtualNodes$production)

  #priceConvergenceArea = area + neighbours in priceConvergenceSystem if area in priceConvergenceSystem
  #init
  antaresData$areas[, priceConvergenceAreaN1 := priceConvergenceAreaN]
  indexToEdit <- antaresData$areas[stringi::stri_detect_fixed(pattern = area,
                                                     str = priceConvergenceSystem),
                                   which=TRUE]

  antaresData$areas[indexToEdit, ':=' (priceConvergenceAreaN1 = paste(sort(unique(c(intersect(strsplit(priceConvergenceSystem, split = " ")[[1]],
                                                                                              strsplit(neighboursN, split = " ")[[1]]),
                                                                                    strsplit(priceConvergenceAreaN, split = " ")[[1]]))),
                                                                      collapse = " ")),
                    by = .(priceConvergenceSystem, priceConvergenceAreaN)]

  #for all index where priceConvergenceArea != priceConvergenceSystem
  #then test if neighbours of neighbours are in priceConvergenceSystem
  #init system
  antaresData$areas[, ':=' (neighboursN = neighbours)]
  antaresData$areas[, ':=' (neighboursN1 = neighbours)]

  #antaresData$areas[, priceConvergenceAreaN1 := priceConvergenceAreaN]
  indexWhereSomethingChange <- c(1, 2)
  i <- 0
  while (length(indexWhereSomethingChange) > 0) {
    indexWhereSomethingChange <- antaresData$areas[priceConvergenceAreaN!=priceConvergenceAreaN1, which=TRUE]

    # iterate N1 := N
    antaresData$areas[indexWhereSomethingChange, priceConvergenceAreaN := priceConvergenceAreaN1]
    antaresData$areas[indexWhereSomethingChange, ':=' (neighboursN = neighboursN1)]

    #compute N1
    antaresData$areas[indexWhereSomethingChange, ':=' (neighboursN1 = paste(getAllNeighbours(neighboursN,
                                                                                             virtualAreas = virtualAreas),
                                                                            collapse = " ")),
                      by = .(neighboursN)]
    antaresData$areas[indexWhereSomethingChange, ':=' (priceConvergenceAreaN1 = paste(sort(unique(c(intersect(strsplit(priceConvergenceSystem, split = " ")[[1]],
                                                                                                              strsplit(neighboursN1, split = " ")[[1]]),
                                                                                                    strsplit(priceConvergenceAreaN, split = " ")[[1]]))),
                                                                                      collapse = " ")),
                      by = .(priceConvergenceSystem, priceConvergenceAreaN)]

    i <- i +1
  }

  antaresData$areas[, priceConvergenceArea := priceConvergenceAreaN1]

  antaresData$areas[, c("priceConvergenceAreaN",
                        "priceConvergenceAreaN1",
                        "neighboursN",
                        "neighboursN1") := NULL]
  invisible(antaresData)
}
