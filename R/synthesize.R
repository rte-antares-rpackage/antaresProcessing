#' Synthesize Monte-Carlo scenarios
#'
#' This function takes as input an object of class \code{antaresData} containing
#' detailed results of a simulation and creates a synthesis of the results.
#' The synthesis contains the average value of each variable over Monte-Carlo
#' scenarios and eventually other aggregated statistics
#'
#' @param x
#'   an object of class \code{antaresData} created with
#'   \code{\link[antaresRead]{readAntares}} and containing detailed results of
#'   an Antares simulation
#' @param ...
#'   Additional parameters indicating which additional statistics to produce.
#'   See details to see how to specify them
#'
#' @return
#' Synthetic version of the input data. It has the same structure as \code{x}
#' except that column \code{mcYear} has been removed. All variables are
#' averaged across Monte-Carlo scenarios.
#'
#' @details
#' Additional statistics can be asked in three different ways:
#'
#' \enumerate{
#'   \item A character string in "min", "max", "std", or "median". It will add
#'     for each column respectively the minimum or maximum value, the standard
#'     deviation or the median of the variable.
#'
#'   \item A named argument whose value is a function. For instance
#'     \code{med = median} will calculate the median of each variable. The name
#'     of the resulting column will be prefixed by "med_".
#'
#'   \item A named argument whose value is a list. It has to contain an element
#'   \code{fun} equal to a function and optionally an element \code{only}
#'   containing the names of the columns to which to apply the function.
#'   For instance \code{med = list(fun = median, only = c("LOAD", "MRG. PRICE"))}
#'   will compute the median of variables "LOAD" and "MRG. PRICE". The result
#'   will be stored in columns "med_LOAD" and "med_MRG. PRICE".
#' }
#'
#' The computation of custom statistics can take some time, especially with hourly
#' data. To improve performance, prefer the third form and compute custom
#' statistics only on a few variables.
#'
#' @examples
#' \dontrun{
#' mydata <- readAntares("all", timeStep = "annual", synthesis = FALSE)
#'
#' synthesize(mydata)
#'
#' # Add minimum and maximum for all variables
#' synthesize(mydata, "min", "max")
#'
#' # Compute a custom statistic for all columns
#' synthesize(mydata, log = function(x) mean(log(x)))
#'
#' # Compute 95% confidence interval for the marginal price
#' synthesize(mydata,
#'            l = list(fun = function(x) quantile(x, 0.025), only = "MRG. PRICE"),
#'            u = list(fun = function(x) quantile(x, 0.975), only = "MRG. PRICE"))
#' }
#'
#'@export
#'
synthesize <- function(x, ...) {
  if (!is(x, "antaresData")) stop("'x' must be an object of class 'antaresData' created with ''readAntares()'")

  if (attr(x, "synthesis") == TRUE) return(x)

  if (is(x, "antaresDataList")) {
    for (n in names(x)) {
      x[[n]] <- synthetise(x[[n]], ...)
    }
    return(x)
  }

  x <- copy(x)
  x$mcYear <- NULL
  idVars <- .idCols(x)
  variables <- setdiff(names(x), idVars)
  attrs <- attributes(x)

  # Compute average of each column
  res <- x[, lapply(.SD, mean), by = idVars]
  .addClassAndAttributes(res, synthesis = FALSE, timeStep = attrs$timeStep,
                         opts = attrs$opts, type = attrs$type)

  # Compute custom statistics
  args <- list(...)

  if (length(args) == 0) return(res)

  aggFun <- vector("list", length(variables))
  names(aggFun) <- variables
  functionNames <- names(args)
  if (is.null(functionNames)) functionNames <- rep("", length(args))

  addFunction <- function(fun, prefix, to) {
    if(is.null(to) || is.na(to)) to <- variables
    for (v in to) {
      if (is.null(aggFun[[v]])) {
        aggFun[[v]] <<- list()
      }
      aggFun[[v]][[paste0(prefix, "_")]] <<- fun
    }
  }

  for (i in 1:length(args)) {
    f <- args[[i]]
    if (is.character(f)) {

      fun <- switch(f, std = sd, min = min, max = max, median = median,
                    stop("Unknown alias ", f))
      addFunction(fun, f, to = variables)

    } else if (is.function(f)) {

      if (functionNames[i] == "") stop("Custom functions must be passed as a named argument.")
      addFunction(f, functionNames[i], to = variables)

    } else if (is.list(f)) {

      if (functionNames[i] == "") stop("Custom functions must be passed as a named argument.")
      if (is.null(f$only)) {
        addFunction(f$fun, functionNames[i], to = variables)
      } else {
        to <- intersect(f$only, variables)
        if (length(to) > 0) {
          addFunction(f$fun, functionNames[i], to = to)
        }
      }

    } else {
      stop("Invalid argument")
    }
  }

  empty <- sapply(aggFun, is.null)
  aggFun <- aggFun[!empty]

  varNames <- lapply(names(aggFun), function(n) paste0(names(aggFun[[n]]), n))
  varNames <- do.call(c, varNames)

  customStats <- x[, as.list(do.call(c, mapply(function(v, funs) {lapply(funs, function(f) f(v))},
                                               v = .SD, funs = aggFun))),
                   keyby = idVars, .SDcols = names(aggFun)]

  setnames(customStats, names(customStats), c(idVars, varNames))

  res <- merge(res, customStats, by = idVars)

  # Modify order of the column
  colnames <- lapply(variables, function(n) {
    paste0(c("", names(aggFun[[n]])), n)
  })
  colnames <- c(idVars, unlist(colnames))
  setcolorder(res, colnames)
  res
}
