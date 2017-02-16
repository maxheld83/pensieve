# QPreSorts ====

#' @title Check and make QPreSorts
#'
#' @export
#'
#' @description Checks and makes QPreSorts
#'
#' @param presorts
#' An integer matrix, with named rows as item handles, named columns as participant names and cells as presorts.
#' `-1L` for `negative`, `0L` for `neutral` and `1L` for `positive`.
#'
#' @template validate
#'
#' @note
#' `presorts` are stored as `integer()` because R does not allow factor matrices.
#' Pre-sorting piles are, of course, `categorical` information and should be treated as such.
#'
#' @family import helpers

QPreSorts <- function(presorts, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  class(presorts) <- append(class(presorts), "QPreSorts")

  # validation first
  if (validate) {
    assert(presorts)
  }
  return(presorts)
}

#' @export
#' @rdname check
check.QPreSorts <- function(x) {
  res <- NULL  # appease R

  res$matrix <- check_matrix(x = x,
                             mode = "integer",
                             any.missing = TRUE,
                             all.missing = FALSE,
                             row.names = "strict",
                             col.names = "strict")
  res$range <- check_integer(x = x,
                             any.missing = TRUE,
                             lower = -2,
                             upper = 2)

  return(report_checks(res = res, info = "QPreSorts"))
}


# QSorts ====

#' @title Check and make QSorts
#'
#' @export
#'
#' @description Checks and makes QSorts
#'
#' @param qsorts An integer array with item handles as first dimension, people as second dimension, arbitrary dimensions thereafter, and item positions in cells.
#' Dimensions must be named.
#'
#' @template validate
#'
#' @family import helpers
QSorts <- function(qsorts, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  qsorts <- classify_clever(x = qsorts, classname = "QSorts")

  if (validate) {
    assert(qsorts)
  }
  return(qsorts)
}


#' @export
#' @rdname check
check.QSorts <- function(x) {
  res <- NULL

  res$array <- check_array(x = x,
                           mode = "integer",
                           any.missing = TRUE,
                           min.d = 2,
                           null.ok = FALSE)
  res <- c(res, check_named_array(x = x))  # via external helper

  return(report_checks(res = res, info = "QSorts"))
}


# QPeopleFeatures ====

#' @title Check and make QPeopleFeatures
#'
#' @export
#'
#' @description Checks and makes QPeopleFeatures, a tibble with arbitrary additional information on the participating people-variables.
#'
#' @param p_feat
#' A tibble, with one row per participant.
#' First column must be the participant names, same as the rownames from [`QSorts`][QSorts].
#'
#' @template validate
#'
#' @family import helpers
QPeopleFeatures <- function(p_feat, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  p_feat <- classify_clever(x = p_feat, classname = "QPeopleFeatures")

  if (validate) {
    assert(p_feat)
  }
  return(p_feat)
}


#' @export
#' @rdname check
check.QPeopleFeatures <- function(x) {
  res <- NULL

  res$tibble <- check_tibble(x = x,
                             types = c("logical", "integer", "integerish", "double", "numeric", "character", "factor"),
                             any.missing = TRUE,
                             all.missing = FALSE,
                             col.names = "strict")
  return(report_checks(res = res, info = "QPeopleFeatures"))
}
