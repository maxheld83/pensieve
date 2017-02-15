#' @title Count distribution of same values per people-variable.
#'
#' @export
#'
#' @import checkmate testthat
#'
#' @description
#' Counts the number of times an item-case is assigned to each value, yields Q distribution for each people-variable.
#'
#' @aliases count_distro diag_distros count_distros
#'
#' @keywords internal
#'
#' @param dataset
#' An integer matrix with item-cases as rows and people-variables as columns.
#'
#' @return
#' An integer matrix with people-variables as rows, values as columns and counts in cells.
#'
#' @examples
#' diag_distros(dataset = civicon_2014$qData$sorts[,,"before"])
#' # results are all the same, because study used forced Q distribution
#'
#' @family distribution helpers

diag_distros <- count_distros <- function(dataset) {

  # Input validation ====
  assert_matrix(x = dataset,
                mode = "integer",
                all.missing = FALSE,
                # notice that we do not, per se, expect named matrices
                null.ok = FALSE)

  # Body ====
  all_distros <- t(apply(X = dataset,
                       MARGIN = 2,
                       FUN = function(x) {
                         table(factor(x = x, levels = min(dataset):max(dataset)))
                       }))
                       # factor hack-fix makes sure 0 counts are included, and results are of same dim so they can be returned as matrix
  names(dimnames(all_distros)) <- c("people", "values")
  return(all_distros)
}


#' @title Same distribution test.
#'
#' @export
#'
#' @description Tests whether the value counts are the same for every people-variable.
#'
#' @aliases is_same
#'
#' @keywords internal
#'
#' @inheritParams diag_distros
#'
#' @return
#' A logical vector of length one, `TRUE` when value counts are the same for all people-variables, else `FALSE`.
#'
#' @note
#' Q data that displays the same value counts for all people-variables is likely to stem from a *forced* distribution, where respondents *had* to fill in all physically available slots in the grid, but this need not be so.
#' Hypothetically, if unlikely, some people-variables might follow exactly the same distributions, even though they had more slots available.
#' For more information, see [diag_forced()].
#'
#' @examples
#' diag_same(dataset = civicon_2014$qData$sorts[,,"before"])
#' # true, because study used forced Q distribution
#'
#' @family distribution helpers

diag_same <- is_same <- function(dataset) {

  # Input validation ====
  # not necessary, done inside count_distros

  # Body ====
  distros <- count_distros(dataset = dataset)  # find distro for each participant
  oneunique <- nrow(unique(distros)) == 1  # test whether all distros are the same
  if (oneunique) {
    same <- all(rowSums(distros) == nrow(dataset))
  } else {
    same <- FALSE
  }
  return(same)
}


#' @title Infer maximum grid of q-sorting grid.
#'
#' @export
#'
#' @description Infers the maximum q-sorting grid from observed dataset.
#'
#' @keywords internal
#'
#' @inheritParams diag_distros
#'
#' @return A positive integer vector of counts of length equal to cover the range of values, named by values.
#'
#' @examples
#' diag_distro_max(dataset = civicon_2014$qData$sorts[,,"before"])
#' # true, because study used forced Q distribution
#'
#' @family distribution helpers

diag_distro_max <- function(dataset) {

  # Input validation ====
  # not necessary, done inside count_distros

  # body ====
  all_distros <- diag_distros(dataset)
  max_distros <- apply(X = all_distros, MARGIN = 2, FUN = function(x) max(x))
  return(max_distros)
}


#' @title Grid test.
#'
#' @export
#'
#' @description Test whether the value counts of all people-variables fall inside the grid.
#'
#' @keywords internal
#'
#' @param grid A positive integer vector of a length covering the range of values, specifying maximum allowed counts for each value.
#' (in Q-parlance, the maximum column heights for the Q-sorts).
#'
#' @inheritParams diag_distros
#'
#' @return A logical vector of length 1, `TRUE` if all people-variables fall inside the grid, else `FALSE`.
#'
#' @examples
#' dataset <- civicon_2014$qData$sorts[,,"before"]
#' diag_inside_grid(dataset = dataset,
#'                  grid = diag_distro_max(dataset))
#' # circularly true!
#'
#' @family distribution helpers

diag_inside_grid <- function(dataset, grid) {
  # input validation ====
  # not necessary for dataset, done inside class (at some point)
  expect_vector(x = grid,
                strict = TRUE,
                any.missing = FALSE,
                all.missing = FALSE,
                names = "unique",
                null.ok = FALSE)
  expect_integerish(x = grid,
                    lower = 0,
                    any.missing = FALSE,
                    all.missing = FALSE,
                    min.len = sum(abs(range(dataset))) + 1,
                    null.ok = TRUE)

  # body ====
  inside_grid <- all(grid >= diag_distro_max(dataset))
  return(inside_grid)
}


#' @title Forced distribution test.
#'
#' @export
#'
#' @description
#' Test whether all people-variables completely fill out the same grid.
#'
#' @keywords internal
#'
#' @param grid
#' A positive integer vector of a length covering the range of values, specifying maximum allowed counts for each value.
#' (in Q-parlance, the maximum column heights for the Q-sorts).
#'
#' @inheritParams diag_distros
#'
#' @details
#' If all people-variables display the same value counts, respondents plausibly faced a *forced* distribution, but this does not strictly speaking follow.
#' It is unlikely, though conceivable, that respondents all adhered to precisely the same distribution, but *could* have used other available slots.
#'
#' Whether, or not, a Q dataset should be considered "forced" can therefore only ascertained from a comparison with the physically available grid during the administration of the Q-sort.
#'
#' In the above, improbable case, the function warns the user of possible misspecification.
#'
#' @return A logical value of length 1, `TRUE` if the distribution is forced, else `FALSE`.
#'
#' @examples
#' dataset <- civicon_2014$qData$sorts[,,"before"]
#' diag_forced(dataset = dataset,
#'             grid = diag_distro_max(dataset))
#'
#' @family distribution diagnostic functions

diag_forced <- function(dataset, grid) {

  # input validation ====
  expect_true(object = diag_inside_grid(dataset = dataset, grid = grid))
  # this takes care of all other input validation downstream

  # body ====
  distros <- diag_distros(dataset = dataset)

  forced <- all(t(distros) == grid)

  # take care of very special case where all distros are same, but apparently not forced
  if (!forced & diag_same(dataset)) {
    warning("All people-variables have the same value counts, but argument grid indicates a forced distribution.
            Make sure this is correct.")
  }
  return(forced)
}
