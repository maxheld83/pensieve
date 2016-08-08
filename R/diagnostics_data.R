#' @title Count distribution of same values per people-variable.
#'
#' @export
#'
#' @import checkmate testthat
#'
#' @description Counts the number of times an item-case is assigned to each value, yields Q distribution for each people-variable.
#'
#' @aliases count_distro diag_distros count_distros
#'
#' @keywords internal
#'
#' @details
#' Helper function, rarely to be used independently.
#' Makes sense only when data are integers.
#'
#' @param dataset An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return An integer matrix with people-variables as rows, values as columns and counts in cells.
#'
#' @examples
#' diag_distros(dataset = civicon_2014$sorts[,,"before"])
#' # results are all the same, because study used forced Q distribution

diag_distros <- count_distros <- function(dataset) {

  # Input validation ====
  assert_matrix(x = dataset, mode = "integer", all.missing = FALSE, row.names = "named", col.names = "named", null.ok = FALSE)

  # Body ====
  all_distros <- t(apply(X = dataset, MARGIN = 2, FUN = function(x) {
    table(x, deparse.level = 2, useNA = "no")
  } ))
  names(dimnames(all_distros)) <- c("people", "values")
  return(all_distros)
}


#' @title Forced distribution test
#'
#' @export
#'
#' @import checkmate testthat
#'
#' @description Tests whether distribution is forced.
#'
#' @aliases is_forced
#'
#' @keywords internal
#'
#' @details
#' Helper function, rarely to be used independently.
#' Makes sense only when data are integers.
#'
#' @param dataset An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return A logical vector of length one, \code{TRUE} when distribution is forced, else \code{FALSE}.
#'
#' @examples
#' diag_forced(data = civicon_2014$sorts[,,"before"])
#' # true, because study used forced Q distribution

diag_forced <- is_forced <- function(data) {

  # Input validation ====
  # not necessary, done inside count_distros

  # Body ====
  distros <- count_distros(data = data)  # find distro for each participant
  same <- nrow(unique(distros)) == 1  # test whether all distros are the same
  if (same) {
    forced <- all(rowSums(distros) == nrow(data))
  } else {
    forced <- FALSE
  }
  return(forced)
}


#' @title Infer maximum grid of q-sorting grid.
#'
#' @export
#'
#' @import checkmate testthat
#'
#' @description Infers the maximum q-sorting grid from observed data.
#'
#' @keywords internal
#'
#' @details
#' Helper function, rarely to be used independently.
#' Makes sense only when data are integers.
#'
#' @param data An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return A positive integer vector of counts of length equal to cover the range of values, named by values.
#'
#' @examples
#' diag_distro_max(data = civicon_2014$sorts[,,"before"])
#' # true, because study used forced Q distribution

diag_distro_max <- function(data) {

  # Input validation ====
  # not necessary, done inside count_distros

  # body ====
  all_distros <- diag_distros(data)
  max_distros <- apply(X = all_distros, MARGIN = 2, FUN = function(x) max(x))
  return(max_distros)
}
