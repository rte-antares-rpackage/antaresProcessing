#' neighbours
#'
#' This function return a list of neighbours.
#' @param antaresData Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}.
#' @param areas A vector with several areas names.
#' @param virtualAreas A vector with several virtual areas names.
#' @param areasString A string with several areas names separated by an espace,
#' see the examples.
#'
#' @return
#' \code{neighbours} return a vector with neighbours areas names.
#' \code{addNeighbours} modifies its input by adding a column neighbours.
#' \code{getAllNeighbours} return a vector with neighbours areas names.
#'
#' @examples
#' \dontrun{
#'
#' res <- neighbours(areas = c("a", "c"),
#' virtualAreas = getAreas("psp"))
#'
#' myData <- readAntares(areas = c("a", "c"), links = getLinks("a"),
#' showProgress = FALSE)
#'
#' addNeighbours(myData)
#'
#' res <- getAllNeighbours(areasString = "a b")
#' }
#' @export
neighbours <- function(areas = NULL, virtualAreas = NULL){
  res <- NULL
  res <- sapply(as.character(areas), FUN =  function(myArea){
    linksNeighbours <- getLinks(areas = myArea, exclude = virtualAreas)
    setdiff(unique(unlist(strsplit(linksNeighbours, split = " - " ))), myArea)
  })
  #init
  neighbourDT <- c(1)
  if(is.null(names(res))){
    DF <- as.data.frame(res)
    areasNamesDT <- names(DF)
    neighbourDT <- data.table::data.table(area = areasNamesDT)
    neighbourDT[, neighbours := paste(DF[[area]],  collapse=" "), by=.(area)]
  }else{
    areasNamesDT <- names(res)
    neighbourDT <- data.table::data.table(area = areasNamesDT)
    neighbourDT[, neighbours := paste(res[[area]],  collapse=" "), by=.(area)]
  }

  return(neighbourDT)
}

#' @rdname neighbours
#' @export
addNeighbours <- function(antaresData = NULL){
  .check_x(antaresData)
  if(!(("area" %in% names(antaresData)) | ("areas" %in% names(antaresData))) ){
    stop("Import areas data.")
  }

  if(!is.null(antaresData$areas)){
    addNeighbours(antaresData$areas)
    return(invisible(antaresData))
  }
  resAttr <- attributes(antaresData)
  virtualAreas <- resAttr$virtualNodes$storageFlexibility

  resNeigh <- neighbours(areas = unique(antaresData$area), virtualAreas = virtualAreas)
  antaresData[resNeigh, neighbours := i.neighbours,
                    on=.(area)]

  invisible(antaresData)
}

#' @rdname neighbours
#' @export
getAllNeighbours <- function(areasString = NULL, virtualAreas = NULL){
  res <- unlist(strsplit(areasString, split = " "));
  neighS <- neighbours(res, virtualAreas = virtualAreas);
  unique(unlist(strsplit(neighS[[2]], split = " ")))
}
