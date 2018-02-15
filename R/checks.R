#' @title Validate S3 classes from pensieve
#'
#' @description Use `check_S3()`, `test()`, `assert()`, `expect()` and `need()` to validate  classed objects from this package.
#'
#' @details
#' The validation functions all use the same underlying [check_S3()] methods, but return their results in one of several forms:
#' - **[checkmate](https://github.com/mllg/checkmate)** package extensions:
#'   - `check_S3()` returns `TRUE` or the error message as a character string,
#'   - `assert()` returns `x` invisibly or throws an error,
#'   - `test()` returns `TRUE` or `FALSE`,
#'   - `expect()` always returns an [testthat::expectation()] for internal use with testing via [`testthat`](https://github.com/hadley/testthat)).
#' - **[shiny](https://shiny.rstudio.com)** validation:
#'   - `need()` returns `NULL` or the error message for interal use with the accio web frontend inside [shiny::validate()].
#' - **validation** inside pensieve:
#'   - `validate_S3()` returns `x` visibly or throws an error.
#'
#' @param x An object with one of the pensieve S3 classes.
#'
#' @param ... further arguments to be passed to methods.
#'
#' @family validation functions
#'
#' @example tests/testthat/helper_checks.R
#'
#' @export
check_S3 <- function(x, ...) {
  # this will be passed to all methods to fill
  ps_coll <- checkmate::makeAssertCollection()
  UseMethod(generic = "check_S3")
}

#' @rdname check_S3
#' @param [`AssertCollection`] ps_coll error collection via [checkmate::makeAssertCollection()], for internal use.
#' @export
#' @noRd
check_S3.default <- function(x, ps_coll = NULL, ...) {
  # this is just a precaution in case this default is called directly, and there is no coll
  if (is.null(ps_coll)) {
    ps_coll <- makeAssertCollection()
  }

  # TODO this might well be a global object, no reason to do this at runtime
  classes <- utils::methods(generic.function = check_S3)
  classes <- stringr::str_replace(
    string = classes,
    pattern = "check_S3.",
    replacement = "")

  # this is hack job necessary, because we need this default to do work *other* than erroring out
  checked <- any(class(x) %in% classes)
  if (!(checked)) {
    stop(
      "Can't find a validation method for this class. ",
      "Maybe this is not a class from pensieve?",
      call. = FALSE
    )
  } else if (ps_coll$isEmpty()) {
    return(TRUE)
  } else {
    msg <- paste0("* ", ps_coll$getMessages())
    msg <- glue::collapse(x = msg, sep = "\n")
    return(msg)
  }
}

#' @rdname check_S3
#' @export
test_S3 <- function(x) {
  UseMethod(generic = "test_S3")
}

#' @rdname check_S3
#' @noRd
#' @export
test_S3.default <- function(x) {
  res <- check_S3(x)
  return(makeTest(res = res))
}

#' @rdname check_S3
#' @inheritParams checkmate::makeExpectation
#' @export
expect_S3 <- function(x, info = NULL, label = NULL) {
  UseMethod(generic = "expect_S3")
}

#' @rdname check_S3
#' @noRd
#' @export
expect_S3.default <- function(x, info = NULL, label = paste(class(x)[1], "S3 class")) {
  res <- check_S3(x)
  return(makeExpectation(x = x, res = res, info = info, label = label))
}

#' @rdname check_S3
#' @inheritParams checkmate::makeAssertion
#' @export
assert_S3 <- function(x, collection = NULL, var.name = NULL) {
  UseMethod(generic = "assert_S3")
}

#' @rdname check_S3
#' @noRd
#' @export
assert_S3.default <- function(x, collection = NULL, var.name = paste(class(x)[1], "S3 class")) {
  res <- check_S3(x)
  return(makeAssertion(x = x, res = res, var.name = var.name, collection = collection))
}

#' @rdname check_S3
#' @export
need_S3 <- function(x, label = NULL) {
  UseMethod(generic = "need_S3")
}

#' @rdname check_S3
#' @noRd
#' @export
need_S3.default <- function(x, label = NULL) {
  res <- check_S3(x)
  return(makeNeed(x = x, res = res, label = label))
}

#' @rdname check_S3
#' @export
validate_S3 <- function(x, collection = NULL, var.name = paste(class(x)[1], "S3 class")) {
  UseMethod(generic = "validate_S3")
}

#' @rdname check_S3
#' @noRd
#' @export
validate_S3.default <- function(x, collection = NULL, var.name = paste(class(x)[1], "S3 class")) {
  assert_S3(x, collection = collection, var.name = var.name)
  return(x)
}


#' @title Validate S3 classes from pensieve
#'
#' @description Use `check()`, `test()`, `assert()`, `expect()` and `need()` to validate  classed objects from this package.
#'
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

# helper: report first error in results, used inside custom checks
report_checks <- function(res, info = NULL) {
  checks <- sapply(X = res, FUN = function(x) {
    isTRUE(x)
  })
  if (all(checks)) {
    return(TRUE)
  } else {
    msg <- paste(if (!(is.null(info))) info,
                 "check on",
                 names(res[!checks][1]),
                 "says:",
                 res[!checks][[1]])
    # below will return extra info as name of string, dicey
        # msg <- structure(res[!checks][[1]],
    #                  names = names(res[!checks][1]))
    return(msg)
  }
}

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
