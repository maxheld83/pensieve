# QItems ====
# this ties it all together in a list, checks for consistency

# QItemSample ====
# logical vector about the inclusion/exclusion of items
# make this a function which *actually* samples

# QItemStrata ====
# logical array of n dimensions, items as rows, arbitrary dimensions,
#' @title Check and make QItemStrata
#'
#' @export
#'
#' @description Checks and makes QItemStrata, the item sampling structure
#'
#' @param strata A logical array of arbitrary dimensions, with first dimension (rows) as item handles, and higher dimensions as orthogonal sampling strata.
#'
#' @template construct
#'
#' @family import helpers
QItemStrata <- function(strata, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  strata <- classify_clever(x = strata, classname = "QItemStrata")

  assert_class2(x = strata, validate = validate)

  return(strata)
}

#' @export
#' @rdname check
check.QItemStrata <- function(x) {
  res <- NULL
  res$array <- check_array(x = x,
                           mode = "logical",
                           any.missing = FALSE,
                           min.d = 1,
                           null.ok = FALSE)
  res <- c(res, check_named_array(x = x))  # via external helper
  return(report_checks(res = res, info = "QItemStrata"))
}

# QConcourse ====
#' @title Check and make QConcourse
#'
#' @description Check and make QConcourse
#'
#' @export
#'
#' @param concourse A character matrix of full items, with named rows as item handles and named columns as languages.
#' Cells can be `NA` when full items are not available.
#' Full items must be unique by columns.
#'
#' @template construct
#'
#' @family import helpers
#'
QConcourse <- function(concourse, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  concourse <- classify_clever(x = concourse, classname = "QConcourse")

  assert_class2(x = concourse, validate = validate)

  return(concourse)
}

#' @export
#' @rdname check
check.QConcourse <- function(x) {
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

  return(report_checks(res = res, info = "QConcourse"))
}

#' @title Custom print method for knitr
#'
#' @description Provides custom print method for knitr.
#' Can also be invoked manually to open interactive outputs in RStudio.
#'
#' @param x a character matrix with full item wording of class [`QConcourse`][QConcourse], as created by [QConcourse()].
#'
#' @template plot
#'
#' @inheritParams knitr::knit_print
#'
#' @export
#'
#' @family knitr output functions
knit_print.QConcourse <- function(x, use_js = NULL, ...) {
  # Input validation ====
  assert_flag(x = use_js,
              na.ok = FALSE,
              null.ok = TRUE)

  x <- classify_clever(x = x, classname = "QConcourse")  # gotta make sure it IS QItems in the first place
  assert(x)

  # Preliminaries ====
  if (is.null(use_js)) {
    use_js <- is_use_js()
  }

  # JS method ====
  if (use_js) {  # interactive
  DT::datatable(data = as.data.frame(x),
                options = list(searchHighlight = TRUE))
  } else {
    print(x)
  }
}


# QItemFeatures ====
# WIDE dataframe with arbitrary features of the items, one row per item


# Drafts and Helpers ====
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
