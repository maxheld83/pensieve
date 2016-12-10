#' @title Check and make QItems.
#'
#' @export
#'
#' @description Checks and makes QItems.
#'
#' @param items A character matrix of full items, with named rows as item handles and named columns as languages.
#' Cells can be \code{NA} when full items are not available.
#' Full items must be unique by columns.
#'
#' @param validate A logical flag, indicating whether the object will be validated.
#' Defaults to \code{TRUE}.
#'
#' @family import helpers
#' @family validation helpers
#'
QItems <- function(items, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  class(items) <- "QItems"

  # validation first
  if (validate) {
    assert(items)
  }
  return(items)
}

#' @export
#' @rdname check
check.QItems <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "character",
                             any.missing = TRUE,
                             all.missing = TRUE,
                             row.names = "strict",
                             col.names = "strict",
                             null.ok = FALSE)

  # check whether the concourse is all unique by columns, as is must be:
  # makes no sense to have same item twice
  # notice that being non-unique by row does not matter:
  # conceivable, though unlikely, that items are same in two languages
  res$unique_by_column <- check_unique_in_column(x = x)

  return(report_checks(res = res, info = "QItems"))
}


# helper: check QLookup
check_QLookup <- check_lookup <- function(x){
  res <- NULL
  res$tibble <- check_tibble(x = x,
                             types = c("integerish", "integer", "character"),
                             any.missing = TRUE,  # some NAs are permissible
                             all.missing = FALSE,
                             null.ok = FALSE)

  res$nna_row <- check_nna_row(x = x[,-1])  # we do not care about first column

  # check whether the lookup table is all unique by columns, as is must be
  # notice the being non-unique by row does not matter
  res$unique_column <- check_unique_in_column(x = x)

  return(report_checks(res = res, info = "lookup"))
}


# helper: check Q_set vs lookup
check_qset_v_lookup <- function(q_set, lookup) {
  if (all(q_set %in% lookup$item_handle)) {
    return(TRUE)
  } else {
    return(paste("All item handles in",
                 vname(q_set),
                 "must be in",
                 vname(lookup)))
  }
}


# TODO this would be a helpful hint for people who only have bad q_set
# #' @importFrom lettercase make_names
# #' @name make_names
# #' @rdname make_names
# #' @export
# NULL
