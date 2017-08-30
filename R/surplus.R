#Copyright © 2016 RTE Réseau de transport d’électricité

#' Compute economic surplus
#'
#' This function computes the economic surplus for the consumers, the producers
#' and the global surplus of an area.
#'
#' @param x
#'   an object of class "antaresDataList" created with the function
#'   \code{readAntares}. It has to contain some areas and all the links that are
#'   connected to these areas. Moreover it needs to have a hourly time step and detailed results.
#' @param timeStep
#'   Desired time step for the result.
#' @param synthesis
#'   If TRUE, average surpluses are returned. Else the function returns surpluses
#'   per Monte-Carlo scenario.
#' @param groupByDistrict
#'   If TRUE, results are grouped by district.
#' @param hurdleCost
#'  If TRUE, HURDLE COST will be removed from congestionFees.
#'
#' @return
#' A data.table with the following columns:
#' \item{area}{Name of the area.}
#' \item{timeId}{timeId and other time columns.}
#' \item{consumerSurplus}{The surplus of the consumers of some area.
#'
#'                      formula = (unsupliedCost[area] - `MRG. PRICE`) * LOAD}
#' \item{producerSurplus}{
#'   The surplus of the producers of some area.
#'
#'   formula = `MRG. PRICE` * production - `OV. COST`
#'
#'   Production includes "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL", "MIX. FUEL",
#'   "MISC. DTG", "H. STOR", "H. ROR", "WIND", "SOLAR" and "MISC. NDG"
#' }
#' \item{rowBalanceSurplus}{
#'   Surplus of the ROW balance.
#'
#'   Formula: `MRG. PRICE` * `ROW BAL.`
#' }
#' \item{storageSurplus}{
#'   Surplus created by storage/flexibility areas.
#'
#'   formula = storage * x$areas$`MRG. PRICE`
#' }
#' \item{congestionFees}{
#'   The congestion fees of a given area. It equals to half
#'   the congestion fees of the links connected to that area.
#'
#'   formula = (congestionFees-hurdleCost) / 2
#'  }
#' \item{globalSurplus}{
#'   Sum of the consumer surplus, the producer surplus and the congestion fees.
#'
#'   formula = consumerSurplus + producerSurplus + storageSurplus + congestionFees + rowBalanceSurplus}
#'
#' @examples
#' \dontrun{
#' showAliases("surplus")
#'
#' mydata <- readAntares(select="surplus")
#' surplus(mydata)
#'
#' surplus(mydata, synthesis = TRUE)
#' surplus(mydata, synthesis = TRUE, groupByDistrict = TRUE)
#' }
#'
#'@export
#'
surplus <- function(x, timeStep = "annual", synthesis = FALSE, groupByDistrict = FALSE, hurdleCost=TRUE) {

  prodVars <- setdiff(pkgEnv$production, "PSP")

  x <- .checkAttrs(x, timeStep = "hourly", synthesis = FALSE)
  x <- .checkColumns(x, list(areas = c("LOAD", "MRG. PRICE", "OV. COST", prodVars, "PSP", "ROW BAL."),
                             links = "CONG. FEE (ALG.)"))

  opts <- simOptions(x)


  # Check that necessary links are present in the object
  areas <- unique(x$areas$area)
  vnodes <- unlist(attr(x, "virtualNodes"))

  neededLinks <- getLinks(areas, exclude = vnodes, opts = opts)

  links <- unique(x$links$link)
  missingLinks <- setdiff(neededLinks, links)
  if (length(missingLinks) > 0) stop("The following links are needed but missing: ",
                                     paste(missingLinks, collapse = ", "))


  # Compute total production of each area
  # For now, we had to the direct production of an area. We add the production of
  # the virtual nodes connected to it.
  allProdVars <- c(prodVars, paste0(prodVars, "_virtual"))
  allProdVars <- intersect(allProdVars, names(x$areas))

  production <- rowSums(x$areas[, allProdVars, with = FALSE])

  # Read unsupplied energy costs
  # unsupliedCost is a named vector. Names are area names and values are the
  # the unsuplied costs of the corresponding areas
  unsupliedCost <- opts$energyCosts$unserved

  # consumer, producer surplus and row balance surplus
  idColsA <- .idCols(x$areas)
  res <- x$areas[,append(mget(idColsA),
                         .(consumerSurplus = (unsupliedCost[areas] - `MRG. PRICE`) * LOAD,
                           producerSurplus = `MRG. PRICE` * production - `OV. COST`,
                           rowBalanceSurplus = `MRG. PRICE` * `ROW BAL.`))]

  # Compute surplus of storage/flexibility
  if (is.null(vnodes)) {
    storageVars <- "PSP"
  } else {
    storageVars <- c("PSP", attr(x, "virtualNodes")$storageFlexibility)
  }
  storage <- rowSums(x$areas[,storageVars, with = FALSE])
  res[, storageSurplus := storage * x$areas$`MRG. PRICE`]

  # Congestion fees
  links <- tstrsplit(neededLinks, split = " - ")
  links <- data.table(link = neededLinks, from = links[[1]], to = links[[2]])
  links <- rbind(links[, .(area = from, link)], links[, .(area = to, link)])
  links <- links[area %in% areas]

  idColsL <- .idCols(x$links)
  if (hurdleCost){
    cong <- merge(links,
                  x$links[, append(mget(idColsL), .(congestionFees = `CONG. FEE (ALG.)` - `HURDLE COST`))],
                  by = "link", allow.cartesian = TRUE)
  }else {
    cong <- merge(links,
                  x$links[, append(mget(idColsL), .(congestionFees = `CONG. FEE (ALG.)`))],
                  by = "link", allow.cartesian = TRUE)
  }

  cong[, link := NULL]
  cong <- cong[, .(congestionFees = sum(congestionFees) / 2), keyby = idColsA]

  res <- merge(res, cong, by = idColsA)

  # Global surplus
  res[, globalSurplus := consumerSurplus + producerSurplus + storageSurplus +
                           congestionFees + rowBalanceSurplus]
  if (groupByDistrict) res <- .groupByDistrict(res, opts)

  # Set correct attributes to the result
  res <- .addClassAndAttributes(res, FALSE, "hourly", opts, type = "surplus")

  res <- changeTimeStep(res, timeStep)

  if (synthesis) res <- synthesize(res)

  res
}
