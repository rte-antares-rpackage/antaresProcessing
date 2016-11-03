#Copyright © 2016 RTE Réseau de transport d’électricité

#' Private function that fills missing values in tables created with
#' readClusterDesc.
#'
#' @param x
#'   data.table created with function readClusterDesc
#' @param ...
#'   named arguments of the form "variable=defaultValue". If the variable is
#'   missing in x, it is added. All missing values are filled with "defaultValue"
#'
#' @return
#' The function modifies by reference its input. For convenience, it invisibly
#' returns 'x'.
#'
#' @noRd

.fillClusterDesc <- function(x, ...) {
  values <- list(...)
  varnames <- names(values)

  for (v in varnames) {
    if (is.null(x[[v]])) x[, c(v) := values[v]]
    else x[is.na(get(v)), c(v) := values[v]]
  }

  invisible(x)
}
