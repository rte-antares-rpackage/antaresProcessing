#' Compute economic surplus
#'
#' This function computes the economic surplus for the consumers, the producers
#' and the global surplus of an area.
#'
#' @param x
#'   an object of class "antaresDataList" created with the function
#'   \code{readAntares}. It has to contain some areas and all the links that are
#'   connected to these areas. Moreover it needs to have a hourly time step.
#' @param timeStep
#'   Desired time step for the result.
#'
#' @return
#' A data.table with the wollowing columns:
#' \item{area}{Name of the area.}
#' \item{timeId}{timeId and other time columns.}
#' \item{consumerSurplus}{The surplus of the consumers of some area.}
#' \item{producerSurplus}{The surplus of the producers of some area.}
#' \item{congestionFees}{The congestion fees of a given area. It equals to half
#'   the congestion fees of the links connected to that area.}
#' \item{globalSurplus}{Sum of the consumer surplus, the producer surplus and
#'   the congestion fees.}
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares(areas = "all", links = "all")
#'
#' mySurplus <- surplus(mydata)
#' }
#'
#'@export
#'
surplus <- function(x, timeStep = "annual") {

  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)

  prodVars <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
                "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR")

  x <- .checkColumns(x, list(areas = c("LOAD", "MRG. PRICE", "OV. COST", prodVars),
                             links = "CONG. FEE (ALG.)"))

  opts <- simOptions(x)

  # Check that necessary links are present in the object
  areas <- unique(x$areas$area)
  neededLinks <- getLinks(areas, regexpSelect = FALSE, opts = opts)

  # Remove links connected to virtualNodes from necessary links
  vnodes <- unlist(attr(x, "virtualNodes"))
  if (!is.null(vnodes)) {
    linksToRemove <- getLinks(vnodes, regexpSelect = FALSE, opts = opts)
    neededLinks <- setdiff(neededLinks, linksToRemove)
  }

  links <- unique(x$links$link)
  missingLinks <- setdiff(neededLinks, links)
  if (length(missingLinks) > 0) stop("The following links are needed but missing: ",
                                     paste(missingLinks, collapse = ", "))


  # Compute total production of each area
  # For now, we had to the total production of an area the production of the
  # virtual nodes connected to it.
  allProdVars <- c(prodVars, paste0(prodVars, "_virtual"))
  if (!is.null(vnodes)) {
    allProdVars <- c(allProdVars, attr(x, "virtualNodes")$storageFlexibility)
  }
  allProdVars <- intersect(allProdVars, names(x$areas))

  production <- rowSums(x$areas[, allProdVars, with = FALSE])

  # Read unsupplied energy costs
  # unsupliedCost is a named vector. Names are area names and values are the
  # the unsuplied costs of the corresponding areas
  unsupliedCost <- unlist(antaresRead:::readIniFile(file.path(opts$inputPath, "thermal", "areas.ini"))$unserverdenergycost)

  # consumer and producer surplus
  idColsA <- intersect(names(x$areas), antaresRead:::pkgEnv$idVars)
  res <- x$areas[,append(mget(idColsA),
                         .(consumerSurplus = (unsupliedCost[areas] - `MRG. PRICE`) * LOAD,
                           producerSurplus = `MRG. PRICE` * production - `OV. COST`))]

  # Congestion fees
  links <- tstrsplit(neededLinks, split = " - ")
  links <- data.table(link = neededLinks, from = links[[1]], to = links[[2]])
  links <- rbind(links[, .(area = from, link)], links[, .(area = to, link)])
  links <- links[area %in% areas]

  idColsL <- intersect(names(x$links), antaresRead:::pkgEnv$idVars)
  cong <- merge(links,
                x$links[, append(mget(idColsL), .(congestionFees = `CONG. FEE (ALG.)`))],
                by = "link", allow.cartesian = TRUE)
  cong[, link := NULL]
  cong <- cong[, .(congestionFees = sum(congestionFees) / 2), keyby = idColsA]

  res <- merge(res, cong, by = idColsA)

  # Global surplus
  res[, globalSurplus := consumerSurplus + producerSurplus + congestionFees]

  # Set correct attributes to the result
  res <- .setAttrs(res, "surplus", opts)

  changeTimeStep(res, timeStep)
}
