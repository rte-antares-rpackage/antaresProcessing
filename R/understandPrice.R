#Copyright © 2016 RTE Réseau de transport d’électricité

#' understandThePrice
#'
#' This function gives some explanations about the price.
#' @param antaresData Object of class \code{antaresData} created with function
#'   \code{\link[antaresRead]{readAntares}}. antaresData must contains hourly detail data
#'   for areas, clusters and links.
#'   Areas data must contains "H. STOR".
#'   Links data must contains transCapacityDirect and transCapacityIndirect.
#'   Clusters data must contains minGenModulation.
#'
#' @return
#' \code{understandPrice} modifies its input by adding to clusters
#' data columns "market.bid.cost" and "must.run".
#'
#' @examples
#' \dontrun{
#' # First simulation
#' studyPath <- "path/to/study/"
#'
#' setSimulationPath(studyPath, 1)
#' myData <- readAntares(showProgress = FALSE,
#'                       mcYears = "all",
#'                       areas = "all",
#'                       clusters = "all")
#' understandThePrice(antaresData = myData)
#'
#' }
#'
#' @export
#'
understandThePrice <- function(antaresData = NULL, opts = simOptions()){

  .check_x(antaresData)

  if(attr(antaresData, "synthesis")){
    stop("antaresData are synthesis, addMonotones() needs detail data.")
  }
  if(is(antaresData, "antaresDataList")){
    for(tableAnta in c("areas", "links", "clusters")){
      if(!(tableAnta %in% names(antaresData))){
        stop("antaresData does not contain areas, clusters and links data")
      }
    }
  }else{
    stop("antaresData must be an antaresDataList with areas, clusters and links data.")
  }

  if(is.null(antaresData$links$congestion)){
    if(is.null(antaresData$links$transCapacityDirect) & is.null(antaresData$links$transCapacityIndirect)){
      stop("antaresData links must contains transCapacityDirect and transCapacityIndirect.")
    }
  }

  if(is.null(antaresData$clusters$minGenModulation)){
    stop("antaresData clusters must contains minGenModulation.")
  }
  if(is.null(antaresData$links$congestion)){
    addLoadFactorLink(antaresData)
  }

  #global variable for this function
  joinArea <- c("area", "mcYear", "timeId")

  # we need column import, export
  if(is.null(antaresData$areas$import)){
    addExportAndImport(antaresData, addCapacities = TRUE, opts = opts)
  }
  #addPriceConvergenceArea to get the cheaper cluster in the system
  if(is.null(antaresData$areas$priceConvergenceArea)){
    addConvergencePriceArea(antaresData)
  }

  clDes <- readClusterDesc(opts = opts)
  .cleanDataCluster(clDes)

  areaInPriceConvergenceArea <- unique(unlist(strsplit(paste(unique(antaresData$areas$priceConvergenceArea),
                                                             collapse = " "), split= " ")))

  for(convergeArea in areaInPriceConvergenceArea){
    realNeighbours <- getAllNeighbours(areasString = convergeArea, virtualAreas = virtualAreas)
    for(nei in realNeighbours){
      if(!(nei %in% unique(antaresData$clusters$area)) & nei %in% unique(clDes$area)){
        stop(paste0("Import clusters data for ' ", nei, " 'to be able to get the cheapest cluster for ' ",
                    convergeArea, " ' "))
      }
    }
  }

  #A compute the last cluster running when there is no hydro
  #A.1 get the market bids of clusters and merge the data

  byJoin <- c("area", "cluster")
  colToAdd <- c("market.bid.cost", "must.run", "unitcount", "nominalcapacity", "group", "spinning")
  antaresData$clusters[clDes,
              (colToAdd) := mget(paste0("i.", colToAdd)),
              on = byJoin]

  #A.2.1 get for each area the most expensive cluster
  antaresData$clusters[is.na(minGenModulation), minGenModulation:=0L]
  antaresData$clusters[is.na(spinning), spinning:=0L]
  antaresData$clusters[, maxProdStarted := as.integer(nominalcapacity*NODU*(1-spinning/100))]
  antaresData$clusters[, remaiCapaStarted := as.integer(maxProdStarted - production)]
  antaresData$clusters[remaiCapaStarted < 0, remaiCapaStarted:=0]
  antaresData$clusters[, minProduction := as.integer(minGenModulation*nominalcapacity*NODU)]
  antaresData$clusters[, marketBid := as.double(market.bid.cost*marketBidModulation)]
  clMostExpensive <- antaresData$clusters[production > 0 ,
                       list(cluster,
                            marketBid,
                            group,
                            minProduction,
                            production,
                            remaiCapaStarted),
                       by = joinArea]

  nameOrigin <- c("cluster", "area", "group", "marketBid")
  nameFinal <- c("marginalCluster", "marginalArea", "marginalGroup", "marBidMarginalCluster")
  #get the cheapest cluster running in the priceConvergenceArea
  clMostExpensive <- clMostExpensive[,.SD[remaiCapaStarted > 0,],
                                     by = .(area, mcYear, timeId)][, .SD[marketBid == min(marketBid)]
                                                                                   , by = .(area, mcYear, timeId)]

  #get the cheaper cluster running in the priceconvergeArea
  clMostExpensive[antaresData$areas, priceConvergenceArea := mget("priceConvergenceArea"),
           on = joinArea]

  convergenceMcYear <- unique(clMostExpensive[, .(priceConvergenceArea), by = .(mcYear, timeId)])


  clMostExpensive[, priceConvergenceArea := convergenceMcYear[stringi::stri_detect_fixed(pattern = area,
                                                                                str = priceConvergenceArea),
                                                     priceConvergenceArea],
        by = .(area, mcYear, timeId)]

  #get a cluster that have a remaining positive capacity  and get the cheaper
  clMostExpensive <- clMostExpensive[,.SD[remaiCapaStarted > 0,],
                                     by = .(priceConvergenceArea, mcYear, timeId)][, .SD[marketBid == min(marketBid)]
                                                                         , by = .(priceConvergenceArea, mcYear, timeId)][
                                                                           , (nameFinal) := mget(nameOrigin),]

  antaresData$areas[clMostExpensive,
                    (nameFinal) := mget(paste0("i.", nameFinal)),
                    on = c("priceConvergenceArea", "mcYear", "timeId")]

  # to explain the price, we cannot take into account timeId when there is storage/prod STEP
  addProdHydroConvergenceArea(antaresData)

  resAttr <- attributes(antaresData)
  colNameVirtualAreas <- resAttr$virtualNodes$storageFlexibility
  #A.3 get the timeIds when thermalCluster make the price (H.STRO = 0 and
  # storage/flex = 0) in all area in convergencePriceArea
  # myDataTimeId <- antaresData$areas[prodHydroConvergeArea==FALSE,
  #                                   mget(c("area", "timeId", "mcYear", colNameVirtualAreas))]
  # for(cName in colNameVirtualAreas){
  #   myDataTimeId <- myDataTimeId[get(cName)==0, ]
  # }
  # timeIdsToSelect <- myDataTimeId[,  .(timeId), by=c("area", "mcYear")]
  #
  # timeIdsToSelect[antaresData$areas, priceConvergenceArea := mget("priceConvergenceArea"),
  #                 on = joinArea]
  #
  # #some times there is stro/flex in one area for one ConvergenceArea
  # timeIdsToSelect[, lengthDim := length(unlist(strsplit(priceConvergenceArea,
  #                                                       split = " " ))),
  #                 by = .(area, mcYear, timeId)]
  #
  # timeIdsToSelect[, realDim := dim(.SD)[1],
  #                 by = .(mcYear, timeId)]
  # timeIdsToSelect[, indexOK := lengthDim==realDim]
  # timeIdsToSelect <- timeIdsToSelect[indexOK==TRUE,]

  for(mcY in unique(antaresData$areas$mcYear)){
      #A.4 the price is made by the last running cluster in the priceConvergenceArea or by a cluster in another
      #area (but we must have remaining capacity of import > 0)
      antaresData$areas[prodFlexConvergeArea==FALSE & prodHydroConvergeArea==FALSE & mcYear==mcY,
                        reasonPrice := paste0("The price is made by the cluster ",
                                              get(nameFinal[1]), " in the area ",
                                              get(nameFinal[2]), ". Its group is : ",
                                              get(nameFinal[3]),  ". Its cost is : ",
                                              get(nameFinal[4]), " euro/Mwh."),
                        by = c("priceConvergenceArea", "mcYear", "timeId")]
    }

  # congestion == 1
  # if all the link are congestionned then the price is determined by a thermal cluster in the area
  #B.1 get the timeIds when thermalCluster make the price (congestion == 1)

  # sapply(as.character(unique(antaresData$areas$area)), FUN = function(myArea){
  #   #all the links from/to an area are congestionned
  #   realLinksOneArea <- getLinks(opts = opts,
  #            areas = myArea,
  #            exclude = c(resAttr$virtualNodes$storageFlexibility,
  #                        resAttr$virtualNodes$production))
  #   #all links must be here
  #   allLinksHere <- TRUE
  #   for(realLink in realLinksOneArea){
  #     if(realLink %in% unique(antaresData$links$link)){
  #       allLinksHere <- allLinksHere & TRUE
  #     }else{
  #       allLinksHere <- allLinksHere & FALSE
  #     }
  #   }
  #   if(allLinksHere){
  #     #get the timeIds where all links are congestionned
  #     congesData <- antaresData$links[link %in% realLinksOneArea,
  #                                     congestion,
  #                                     by = .(link, timeId, mcYear)]
  #     #if no row, do nothing
  #     nRowCongestionned <- congesData[, .N, ]
  #     if(nRowCongestionned > 0){
  #       resLinksAllCongestionned <- congesData[, allLinksCon := min(congestion),
  #                                              by = .(timeId, mcYear)][,c("congestion", "link"):=NULL]
  #       resLinksAllCongestionned[, area := myArea]
  #       resLinksAllCongestionned <- unique(resLinksAllCongestionned)
  #       antaresData$areas[resLinksAllCongestionned,
  #                         ("allLinksCon") := mget(paste0("i.", "allLinksCon")),
  #                         on = .(area, timeId, mcYear)]
  #       antaresData$areas[area==myArea & is.na(allLinksCon), allLinksCon:=0]
  #       #TODO
  #       #for this area, when allLinksCon is equal to 1 then determie which cluster is the more expensive
  #       # for this timeIds
  #       # get the timeIds and get the most expensiveCluster
  #       nRowRealCongestion <- antaresData$areas[area==myArea & allLinksCon==1, .N, ]
  #       if(nRowRealCongestion > 0){
  #         myDataTimeId <- antaresData$areas[allLinksCon==1 & `H. STOR`==0 &
  #                                             area==myArea, mget(c("area",
  #                                                                  "timeId",
  #                                                                  "mcYear",
  #                                                                  colNameVirtualAreas))]
  #         for(cName in colNameVirtualAreas){
  #           myDataTimeId <- myDataTimeId[get(cName)==0, ]
  #         }
  #         timeIdsToSelect <- myDataTimeId[,  .(timeId), by=c("area", "mcYear")]
  #         if(timeIdsToSelect[, .N] > 0){
  #           #B.4 the price is made by the last running cluster in the area
  #           for(mcY in unique(timeIdsToSelect$mcYear)){
  #             antaresData$areas[timeId %in% timeIdsToSelect[area==myArea & mcYear==mcY, (timeId)]
  #                               & area == myArea & mcYear==mcY,
  #                               reasonPrice := paste0("The price is made by the cluster ",
  #                                                     nameFinal[1], " in the ",
  #                                                     nameFinal[2], " group. Its cost is ",
  #                                                     nameFinal[3], " euro/Mwh."), by=c("area", "mcYear", "timeId")]
  #           }
  #         }
  #       }
  #     }
  #   }
  # })

  # print a report
  numberLines <- antaresData$areas[, .N]
  numberLinesExplain <- antaresData$areas[!is.na(reasonPrice), .N]
  message("Percentage of lines explained : ",
          as.character(round(numberLinesExplain/numberLines*100)), "%")
  invisible(antaresData)
}

addProdHydroConvergenceArea <- function(antaresData = NULL){
  antaresData$areas[, maxHydroConvergeArea := max(`H. STOR`),
                    by = c("priceConvergenceArea", "mcYear", "timeId")]
  antaresData$areas[, prodHydroConvergeArea := ifelse(maxHydroConvergeArea>0,
                                                      TRUE,
                                                      FALSE)]

  resAttr <- attributes(antaresData)
  colNameVirtualAreas <- resAttr$virtualNodes$storageFlexibility
  antaresData$areas[, maxProdStorVirtual := do.call(pmax, .SD[,mget(colNameVirtualAreas)]),
                 by = c("priceConvergenceArea", "mcYear", "timeId")]
  antaresData$areas[, minProdStorVirtual := do.call(pmin, .SD[,mget(colNameVirtualAreas)]),
                 by = c("priceConvergenceArea", "mcYear", "timeId")]
  antaresData$areas[, absMinMaxStorVirtual := max(abs(minProdStorVirtual),
                                               maxProdStorVirtual),
                 by = c("priceConvergenceArea", "mcYear", "timeId")]

  antaresData$areas[, prodFlexConvergeArea := ifelse(absMinMaxStorVirtual>0,
                                                      TRUE,
                                                      FALSE)]
  invisible(antaresData)
}
