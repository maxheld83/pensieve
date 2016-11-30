#' @title Check and make qItems.
#'
#' @export
#'
#' @description Checks and makes qItems from concourse, q-set and optional lookup table.
#'
#' @param q_set A character vector of item handles.
#' Handles must be unique and valid R names.
#' Handles must be a subset of \code{concourse}.
#'
#' @param concourse A character matrix of full items, with named rows as item handles and named columns as languages.
#' Defaults to \code{NULL}, in which case full items are not provided.
#' Full items must be unique by columns.
#'
#' @param lookup A data frame or tibble of manually defined item IDs with the first column as item handles.
#' Can have several columns of alternative item IDs.
#' Item IDs must be unique by columns.
#' All item handles from \code{q_set} must be defined.
#' Defaults to \code{NULL}, in which case items are automatically identified (recommended).
#'
#' @details
#' The basis of a Q study is a concourse of possible items, from which \emph{some} items are sampled into the Q-Set.
#' Q-Set and concourse are sometimes accompanied by manual lookup table, though automatic procedures are recommended.
#' These three objects are here tested for internal consistency, and combined into the canonical \code{qItems} S3 object.
#'
#' @return
#' A character vector of item handles, all as unique and valid R names.
#'
#' @family import
#'

qItems <- function(q_set, concourse = NULL, lookup = NULL) {
  # check first
  # TODO replace this with appropriate expect_ function, once those are operational
  res <- check_qItems(q_set = q_set, concourse = concourse, lookup = lookup)
  if (!isTRUE(res)) {
    stop(res)
  }

  structure(list(q_set = q_set, concourse = concourse, lookup = lookup), class = "qItems")
}

#' @export
#' @rdname qItems

check_qItems <- function(q_set, concourse = NULL, lookup = NULL) {

  # Input validation ====
  res <- NULL  # appease R

  # check CONCOURSE
  res$concourse <- check_matrix(x = concourse,  # check the matrix
                                mode = "character",  # must be text only
                                any.missing = FALSE,
                                row.names = "strict",
                                col.names = "strict",
                                null.ok = TRUE)

  # helper: check unique by column
  check_unique_in_column <- function(x, name = "input") {
    duplicates <- apply(X = x, MARGIN = 2, FUN = function(x) {
      duplicated(x = x)
    })
    if (any(duplicates)) {
      return(paste(name, "must only have unique entries by column."))
    } else {
      return(TRUE)
    }
  }

  # check whether the concourse is all unique by columns, as is must be
  # notice the being non-unique by row does not matter
  if (!is.null(concourse)) {  # only applies if not null
    res$concourse_unique <- check_unique_in_column(x = concourse,
                                                   name = "concourse")
  }

  # check QSET
  res$q_set <- check_vector(x = q_set,
                            strict = TRUE,
                            any.missing = FALSE,
                            unique = TRUE,
                            names = "unnamed",
                            null.ok = TRUE)

  # check LOOKUP
  res$lookup <- check_tibble(x = lookup,
                             types = c("integerish", "integer", "character"),
                             any.missing = FALSE,
                             all.missing = FALSE,
                             null.ok = TRUE)

  # check whether the lookup table is all unique by columns, as is must be
  # notice the being non-unique by row does not matter
  if (!is.null(lookup)) {  # only applies if not null
    res$lookup_unique <- check_unique_in_column(x = lookup, name = "lookup")
  }

  # Consistency checks ====
  if (!is.null(concourse)) { # check concourse vs q_set
    res$q_set_vs_concourse <- check_subset(x = q_set,
                                           choices = rownames(concourse),
                                           empty.ok = FALSE)
  }
  if (!is.null(lookup)) {  # check lookup and q_set
    res$q_set_vs_lookup <- check_subset(x = q_set,
                                        choices = unclass(lookup[,1])[[1]],
                                        empty.ok = FALSE)
  }

  # return ====
  # preliminary step is necessary b/c res is list and may contain logical AND character strings
  all_res <- sapply(X = res, FUN = function(x) {isTRUE(x)})  # now this is always a logical vector
  if (all(all_res)) {
    return(TRUE)  # everyone is happy!
  } else {
    return(res[!all_res][[1]])  # let's just take the first error, one at a time
  }
}

# #' @export
# #' @rdname assert_qItems
# assert_qItems <- makeAssertionFunction(check.fun = check_qItems)

# #' @export
# #' @rdname test_qItems
# test_qItems <- makeTestFunction(check.fun = check_qItems)

# #' @export
# #' @rdname expect_qItems
# expect_qItems <- makeExpectationFunction(check.fun = check_qItems)

# #' @importFrom lettercase make_names
# #' @name make_names
# #' @rdname make_names
# #' @export
# NULL
