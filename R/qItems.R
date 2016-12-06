#' @title Check and make QItems.
#'
#' @export
#'
#' @description Checks and makes QItems from concourse, q-set and optional lookup table.
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
#' @param validate A logical flag, indicating whether the object will be validated.
#' Defaults to \code{TRUE}.
#'
#' @details
#' The basis of a Q study is a concourse of possible items, from which \emph{some} items are sampled into the Q-Set.
#' Q-Set and concourse are sometimes accompanied by a manual lookup table, though automatic procedures are recommended.
#' These three objects are here tested for internal consistency, and combined into the canonical \code{QItems} S3 object.
#'
#' @family import helpers
#' @family validation helpers
#'
#' @examples
# construct QItems class like so
#' QItems(q_set = civicon_2014$QItems$q_set,
#'        concourse = civicon_2014$QItems$concourse,
#'        lookup = civicon_2014$QItems$lookup,
#'        validate = TRUE)  # strongly recommended
#' QItems(q_set = c("sta1", "sta2"))  # also works with minimal information
#'
#' # can also be checked, expected, tested, asserted
#' check(civicon_2014$QItems)
#'
#' # gives informative error messages when validation fails
#' badobject <- QItems(q_set = NA, validate = FALSE)  # Don't do this at home
#' \dontrun{check(badobject)}  # fails because q_set cannot be only NAs
#'
QItems <- function(q_set, concourse = NULL, lookup = NULL, validate = TRUE) {
  x <- structure(list(q_set = q_set, concourse = concourse, lookup = lookup),
                   class = "QItems")

  # validation first
  if (validate) {
    expect(x)
  }

  return(x)
}

#' @export
#' @rdname check
check.QItems <- function(x) {
  # Input validation ====
  res <- NULL  # appease R

  # check names
  res$list <- check_list(x = x, names = "strict", len = 3)
  res$names <- check_subset(x = names(x), choices = c("q_set",
                                                      "concourse",
                                                      "lookup"))

  q_set <- x$q_set # makes below code easier to read
  concourse <- x$concourse
  lookup <- x$lookup

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
      duplicated(x = x, incomparables = NA)
    })
    if (any(duplicates)) {
      return(paste(name, "must only have unique entries by column."))
    } else {
      return(TRUE)
    }
  }

  # check whether the concourse is all unique by columns, as is must be
  # notice that being non-unique by row does not matter
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
                             any.missing = TRUE,  # some NAs are permissible
                             all.missing = FALSE,
                             null.ok = TRUE)

  if (!is.null(lookup)) {
    # check whether the lookup table has at least one-non-NA entry per row
    if (all(rowSums(x = is.na(lookup)) < ncol(lookup) - 1)) {
      res$lookup_na <- TRUE
    } else {
      res$lookup_na <- "Lookup table must not have only NAs in a row."
    }

    # check whether the lookup table is all unique by columns, as is must be
    # notice the being non-unique by row does not matter
    res$lookup_unique <- check_unique_in_column(x = lookup, name = "lookup")

    # CONSISTENCY lookup and q_set
    res$q_set_vs_lookup <- check_subset(x = q_set,
                                        choices = unclass(lookup[,1]),
                                        empty.ok = FALSE)
  }

  # Consistency checks ====
  if (!is.null(concourse)) { # check concourse vs q_set
    res$q_set_vs_concourse <- check_subset(x = q_set,
                                           choices = rownames(concourse),
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

# #' @importFrom lettercase make_names
# #' @name make_names
# #' @rdname make_names
# #' @export
# NULL
