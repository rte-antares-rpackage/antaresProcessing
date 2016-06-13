.groupByDistrict <- function(x, opts, fun = c(sum)) {
  x <- merge(x, opts$districtsDef, by = "area", allow.cartesian = TRUE)

  # Check that all nodes from a district are in the data
  areasInData <- unique(x$area)
  districts <- intersect(x$district, opts$districtsDef$district)
  districtsDef <- split(opts$districtsDe$area, opts$districtsDef$district)

  for (d in districts) {
    missingAreas <- setdiff(districtsDef[[d]], areasInData)

    if (length(missingAreas) > 0) warning("The following areas belongs to district ", d, " but are not in 'x': ",
                                          paste(missingAreas, collapse = ", "))
  }

  # Aggregation by district
  x[, area := NULL]
  idVars <- .idCols(x)

  if (length(fun) == 1) fun <- rep(fun, ncol(x) - length(idVars))

  x[, mapply(function(x, f) {f(x)}, x = .SD, f = fun, SIMPLIFY=FALSE),
    by = idVars]

}
