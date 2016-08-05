#' @title Count distribution per people-variable
#'
#' @export
#'
#' @import checkmate testthat
#'
#' @description Counts the number of times an item-case is assigned to each value, yields Q distribution for each people-variable.
#'
#' @aliases count_distro
#'
#' @keywords internal
#'
#' @details
#' Helper function, rarely to be used independently.
#' Makes sense only when data are integers.
#'
#' @param data An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return A list of dataframes, one for each people-variable, each with a column for the value (\code{x}) and the frequency (\code{freq})-
#'
#' @examples
#' count_distros(data = civicon_2014$sorts[,,"before"])
#' # results are all the same, because study used forced Q distribution

count_distros <- function(data) {

  # Input validation ====
  assert_matrix(x = data, mode = "integer", all.missing = FALSE, row.names = "named", col.names = "named", null.ok = FALSE)

  # Body ====
  all_distros <- apply(X = data, MARGIN = 2, FUN = function(x) {plyr::count(x)})
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
#' @keywords internal
#'
#' @details
#' Helper function, rarely to be used independently.
#' Makes sense only when data are integers.
#'
#' @param data An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return A logical vector of length one, \code{TRUE} when distribution is forced.
#'
#' @examples
#' is_forced(data = civicon_2014$sorts[,,"before"])
#' # true, because study used forced Q distribution

is_forced <- function(data) {

  # Input validation ====
  # not necessary, done inside count_distros

  # Body ====
  return(length(unique(count_distros(data))) == 1)
}
