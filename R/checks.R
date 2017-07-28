#' @title Validate S3 classes from this package.
#' @description Use `check()`, `test()`, `assert()` and `expect()` to validate  classed objects from this package.
#' @export
#' @inheritParams checkmate::makeAssertion
#' @inheritParams checkmate::makeExpectation
#' @template check
check <- function(x) {
  UseMethod("check")
}

#' @rdname check
#' @export
test <- function(x) {
  UseMethod("test")
}

#' @rdname check
test.default <- function(x) {
  res <- check(x)
  return(makeTest(res = res))
}

#' @rdname check
#' @export
expect <- function(x, info = NULL, label = NULL) {
  UseMethod("expect")
}

#' @rdname check
expect.default <- function(x, info = NULL, label = paste(class(x), "S3 class")) {
  res <- check(x)
  return(makeExpectation(x = x, res = res, info = info, label = label))
}

#' @rdname check
#' @export
assert <- function(x, collection = NULL, var.name = NULL) {
  UseMethod("assert")
}

#' @rdname check
assert.default <- function(x, collection = NULL, var.name = paste(class(x), "S3 class")) {
  res <- check(x)
  return(makeAssertion(x = x, res = res, var.name = var.name, collection = collection))
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


# helper: check unique by column ===
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


# helper: check whether some array is all named ====
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

