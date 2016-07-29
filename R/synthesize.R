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
#'   an Antares simulation.
#' @param ...
#'   Additional parameters indicating which additional statistics to produce.
#'   See details to see how to specify them.
#' @param prefixForMeans
#'   Prefix to add to the columns containing average values. If it is different
#'   than "", a "_" is automatically added.
#'
#' @return
#' Synthetic version of the input data. It has the same structure as \code{x}
#' except that column \code{mcYear} has been removed. All variables are
#' averaged across Monte-Carlo scenarios and eventually some additional columns
#' have been added corresponding to the requested custom statistics.
#'
#' @details
#' Additional statistics can be asked in three different ways:
#'
#' \enumerate{
#'   \item A character string in "min", "max", "std", "median" or "qXXX" where
#'     "XXX" is a real number between 0 and 100. It will add
#'     for each column respectively the minimum or maximum value, the standard
#'     deviation, the median or a quantile.
#'
#'   \item A named argument whose value is a function or one of the previous
#'     aliases. For instance \code{med = median} will calculate the median of
#'     each variable. The name of the resulting column will be prefixed by
#'     "med_". Similarly, \code{l = "q5"} will compute the 5%% quantile of
#'     each variable and put the result in a column with name prefixed by "l_"
#'
#'   \item A named argument whose value is a list. It has to contain an element
#'   \code{fun} equal to a function or an alias and optionally an element
#'   \code{only} containing the names of the columns to which to apply the function.
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
#' synthesize(mydata, log = function(x) mean(log(1 + x)))
#'
#' # Same but only for column "LOAD"
#' synthesize(mydata,
#'            log = list(fun = function(x) mean(log(1 + x)),
#'                       only = "LOAD"))
#'
#' # Compute the proportion of time balance is positive
#'
#' synthesize(mydata, propPos = list(fun = function(x) mean(x > 0),
#'                                   only = "BALANCE"))
#'
#' # Compute 95% confidence interval for the marginal price
#' synthesize(mydata,
#'            l = list(fun = "q2.5", only = "MRG. PRICE"),
#'            u = list(fun = "q97.5", only = "MRG. PRICE"))
#' }
#'
#'@export
#'
synthesize <- function(x, ..., prefixForMeans = "") {
  if (!is(x, "antaresData")) stop("'x' must be an object of class 'antaresData' created with ''readAntares()'")

  if (attr(x, "synthesis") == TRUE) return(x)

  if (is(x, "antaresDataList")) {
    for (n in names(x)) {
      x[[n]] <- synthesize(x[[n]], ...)
    }
    return(x)
  }

  x <- copy(x)
  x$mcYear <- NULL
  idVars <- .idCols(x)
  variables <- setdiff(names(x), idVars)
  attrs <- attributes(x)

  # Compute average values of each column
  res <- suppressWarnings(x[, lapply(.SD, mean), by = idVars])
  if (prefixForMeans != "") {
    setnames(res, variables, paste0(prefixForMeans, "_", variables))
  }
  .addClassAndAttributes(res, synthesis = TRUE, timeStep = attrs$timeStep,
                         opts = attrs$opts, type = attrs$type)

  # Determine the list of custom statistics to compute for each variable in the
  # input data. aggFun contains one element per variable which is a named list
  # of variables
  args <- list(...)

  if (length(args) == 0) return(res)

  aggFun <- vector("list", length(variables))
  names(aggFun) <- variables

  # When arguments are named, the name of the argument is used as a prefix for
  # the corresponding custom variable
  functionNames <- names(args)
  if (is.null(functionNames)) functionNames <- rep("", length(args))

  # Register a custom statistic function for a set of variables.
  addFunction <- function(fun, prefix, to) {
    if (is.character(fun)) {
      if (grepl("^q\\d+(\\.\\d+)?$", fun)) {
        q <- as.numeric(substring(fun, 2))
        fun <- function(x) quantile(x, probs = q / 100)
      } else {
        fun <- switch(f, std = sd, min = min, max = max, median = median,
                      stop("Unknown alias ", f))
      }
    }

    for (v in to) {
      if (is.null(aggFun[[v]])) {
        aggFun[[v]] <<- list()
      }
      aggFun[[v]][[paste0(prefix, "_")]] <<- fun
    }
  }

  # Loop over arguments
  for (i in 1:length(args)) {
    f <- args[[i]]
    # if (is.character(f)) {
    #   if (grepl("^q\\d+(\\.\\d+)?$", f)) {
    #     q <- as.numeric(substring(f, 2))
    #   }
    #
    #
    #   addFunction(fun, f, to = variables)
    #
    # } else if (is.function(f)) {
    #
    #   if (functionNames[i] == "") stop("Custom functions must be passed as a named argument.")
    #   addFunction(f, functionNames[i], to = variables)

    if (is.list(f)) {

      if (functionNames[i] == "") stop("Custom functions must be passed as a named argument.")
      if (is.null(f$only)) {
        addFunction(f$fun, functionNames[i], to = variables)
      } else {
        to <- intersect(f$only, variables)
        if (length(to) > 0) {
          addFunction(f$fun, functionNames[i], to = to)
        }
      }

    } else if (functionNames[i] != "" & (is.character(f) | is.function(f))) {

      addFunction(f, functionNames[i], to = variables)

    } else if (is.character(f)) {

      addFunction(f, f, to = variables)

    } else stop("Invalid argument")
  }

  # Keep only variables for wich we want to compute custom statistics.
  empty <- sapply(aggFun, is.null)
  aggFun <- aggFun[!empty]
  aggFun <- aggFun[names(aggFun) %in% variables]

  if (length(aggFun) > 0) {
    # Name of the custom columns: prefix + "_" + variable name
    varNames <- lapply(names(aggFun), function(n) paste0(names(aggFun[[n]]), n))
    varNames <- do.call(c, varNames)

    # Compute the custom statistics
    customStats <- x[, unname(as.list(do.call(c, mapply(function(v, funs) {lapply(funs, function(f) f(v))},
                                                 v = .SD, funs = aggFun)))),
                     keyby = idVars, .SDcols = names(aggFun)]

    setnames(customStats, names(customStats), c(idVars, varNames))

    # Merge with average statistics
    res <- merge(res, customStats, by = idVars)

    # Modify order of the columns in order to group columns corresponding to the
    # same variable.
    colnames <- lapply(variables, function(n) {
      prefMean <- if (prefixForMeans == "") "" else paste0(prefixForMeans, "_")
      prefixes <- c(prefMean, names(aggFun[[n]]))
      paste0(prefixes, n)
    })
    colnames <- c(idVars, unlist(colnames))
    setcolorder(res, colnames)
  }

  res
}
