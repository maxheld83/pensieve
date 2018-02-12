#' @title Validate S3 classes from this package.
#' @description Use `check()`, `test()`, `assert()`, `expect()` and `need()` to validate  classed objects from this package.
#' @export
#' @inheritParams checkmate::makeAssertion
#' @inheritParams checkmate::makeExpectation
#' @template check
check <- function(x) {
  UseMethod(generic = "check")
}

#' @rdname check
#' @export
test <- function(x) {
  UseMethod(generic = "test")
}

#' @rdname check
#' @export
test.default <- function(x) {
  res <- check(x)
  return(makeTest(res = res))
}

#' @rdname check
#' @export
expect <- function(x, info = NULL, label = NULL) {
  UseMethod(generic = "expect")
}

#' @rdname check
#' @export
expect.default <- function(x, info = NULL, label = paste(class(x), "S3 class")) {
  res <- check(x)
  return(makeExpectation(x = x, res = res, info = info, label = label))
}

#' @rdname check
#' @export
assert <- function(x, collection = NULL, var.name = NULL) {
  UseMethod(generic = "assert")
}

#' @rdname check
#' @export
assert.default <- function(x, collection = NULL, var.name = paste(class(x)[1], "S3 class")) {
  res <- check(x)
  return(makeAssertion(x = x, res = res, var.name = var.name, collection = collection))
}

#' @rdname check
#' @export
need <- function(x, label = NULL) {
  UseMethod(generic = "need")
}

#' @rdname check
#' @export
need.default <- function(x, label = NULL) {
  res <- check(x)
  return(makeNeed(x = x, res = res, label = label))
}

# helper: make need function for use in shiny::validate()
# this always returns NULL (if successful), string (if invalid object) or FALSE (if other problem)
# no checkmate function does this, so we have to make it
# TODO this is just a placeholder until checkmate ships the real deal: https://github.com/mllg/checkmate/issues/118
makeNeedFunction <- function(check.fun) {
  function(x, ...) {
    if (is.null(x)) {
      return(FALSE)
    } else if (isTRUE(check.fun(x, ...))) {
      return(NULL)
    } else {
      return(check.fun(x, ...))
    }
  }
}
# and the simpler variant
makeNeed <- function(x, res, label) {
  if (is.null(x)) {
    return(FALSE)
  } else if (isTRUE(res)) {
    return(NULL)
  } else {
    return(paste0(label, ": ", res))
  }
}

#' @title Report checks
#'
#' @description Turns (nested) list of error messages into a check.
#'
#' @param res A (nested) list of error messages.
#'
#' @param info A character string, name of the object or additional information.
#'
#' @details
#' If elements are named, that name will be included in the check as the offending object.
#' If elements are unnamed, they will be itemized.
#' Each nesting level will be intended.
#'
#' @noRd
report_checks <- function(res, info = NULL) {
  msg <- purrr::discard(.x = res$babel_language, .p = isTRUE)

  # report msg here
  # use map_if here to cut code
}
# example error msg
res <- list(
  a = TRUE,
  b = "error msg 1",
  c = list(
    TRUE,
    "error msg 2"
  ),
  d = list(
    e = "error msg 3",
    "error msg 4",  # no name for this list item
    f = list(
      g = list(
        h = "error msg 5",
        i = TRUE
      )
    )
  )
)
flatten_msg_hrz <- function(msg) {
  msg <- purrr::discard(.x = msg, .p = isTRUE)
  msg <- purrr::imap_chr(.x = msg, .f = function(x, y) {
    if (test_character(x = y,
                       min.chars = 1,
                       any.missing = FALSE,
                       all.missing = FALSE,
                       len = 1,
                       null.ok = FALSE)) {
      if (is.null(x)) {
        return(collapse(y))
      } else {
        return(glue(y, ": ", x))
      }
    } else {
      if (is.null(x)) {
        return("crickets!")
      } else {
        return(x)
      }
    }
  })
  msg <- glue::collapse(x = msg, sep = ",")
  return(msg)
}

flatten_msg_ver <- function(msg) {
  depth <- purrr::vec_depth(msg)
  while (depth > 2) {
    msg <- purrr::modify_depth(.x = res, .depth = 5, .f = is.null, .ragged = TRUE)
    depth <- purrr::vec_depth(msg)
  }
  flatten_msg_hrz(msg)
  return(msg)
}
# remember that rapply does not work, b/c it never knows the name of the object

# custom checks ====

# helper: check whether table has at least one none-NA entry per row
check_nna_row <- function(x) {
  if (all(rowSums(x = is.na(x)) < ncol(x))) {
    res <- TRUE
  } else {
    res <- "Must not have only NAs in a row."
  }
  return(res)
}
expect_nna_row <- checkmate::makeExpectationFunction(check.fun = check_nna_row)
test_nna_row <- checkmate::makeTestFunction(check.fun = check_nna_row)
assert_nna_row <- checkmate::makeAssertionFunction(check.fun = check_nna_row)


# helper: check unique by column
check_unique_in_column <- function(x) {
  duplicates <- apply(X = x, MARGIN = 2, FUN = function(x) {
    duplicated(x = x, incomparables = NA)
  })
  if (any(duplicates)) {
    return("must only have unique entries by column")
  } else {
    return(TRUE)
  }
}
assert_unique_in_column <- checkmate::makeAssertionFunction(check.fun = check_unique_in_column)
test_unique_in_column <- checkmate::makeTestFunction(check.fun = check_unique_in_column)
expect_unique_in_column <- checkmate::makeExpectationFunction(check.fun = check_unique_in_column)


# helper: check whether a is subset of b ===
# check_names_many <- function(x, type = "named", permutation.of = NULL, subset.of = NULL, identical.to = NULL) {
#   res <- check_names(x = x, type = type, permutation.of = permutation.of, subset.of = subset.of, identical.to = identical.to)
#   if (!isTRUE(res)) {
#     if (!is.null(permutation.of)) {
#       return(paste(vname(x), "must be permutation of", vname(permutation.of)))
#     } else if (!is.null(subset.of)) {
#       return(paste(vname(x), "must be subset of", vname(subset.of)))
#     } else if (!is.null(identical.to)) {
#       return(paste(vname(x), "must be identical to", vname(identical.to)))
#     }
#   } else {
#     return(TRUE)
#   }
# }
# obj1 <- c("foo", "bar")
# obj2 <- c("foo", "baz")
# check_names_many(x = obj2, identical.to = obj1)
# assert_names_many <- makeAssertionFunction(check.fun = check_names_many)


# # helper: just an idea for gh
# check_consistency(x,  # object 1,
#                   y,  # object 2,
#                   # all of the following conditions must be read as: x blah-condition y
#                   length = TRUE,  # could also be "smaller", "larger"
#                   # interpreted as, e.g. x smaller y
#                   names = "subset.of",
#                   # interpreted as, e.g. x subset.of y
#                   ncol = TRUE,  # could also be "smaller"
#                   nrow = TRUE,
#                   colnames = "identical.to",  # etc
#                   rownames = "identical.to", # etc
# ) {
#   # here be dragons
#   return(paste(vname(x), "must be shorter than", vname(y)))  # etc.
# }


# helper: check whether some array is all named
check_named_array <- function(x) {
  res <- NULL

  # this checks whether the dimnames are named!
  res$names_dimnames <- check_names(x = names(dimnames(x)),
                                    type = "strict")

  # AND whether the dims are named
  for (i in length(dim(x))) {
    res[[paste0("names_dim_", i)]] <- check_names(x = dimnames(x)[[i]],
                                                  type = "strict")
  }
  return(res)
}

# helper: if there are names, make sure they are strict
check_names2 <- function(x, type = "strict", ...) {
  if (is.null(x)) {
    return(TRUE)
  } else {
    check_names(x = x, type = type, ...)
  }
}
assert_names2 <- checkmate::makeAssertionFunction(check.fun = check_names2)
test_names2 <- checkmate::makeTestFunction(check.fun = check_names2)
expect_names2 <- checkmate::makeExpectationFunction(check.fun = check_names2)

# helper: check whether vector has 0 variance
# this is sometimes happens, and makes no sense for our purposes
check_var <- function(x) {
  if (isTRUE(stats::var(x = x, na.rm = TRUE) == 0)) {
    return("must have non-zero variance")
  } else {
    return(TRUE)
  }
}
assert_var <- checkmate::makeAssertionFunction(check.fun = check_var)
test_var <- checkmate::makeTestFunction(check.fun = check_var)
expect_var <- checkmate::makeExpectationFunction(check.fun = check_var)
