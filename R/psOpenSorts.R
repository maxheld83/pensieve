# helper ====
#' @title Store *multiple* open sorts in a psOpenSorts list.
#'
#' @aliases psLogicalOpenSorts psOrdinalOpenSorts psIntervalOpenSorts
#'
#' @details
#' Open sorting categorizations *cannot* be compared between participants, because each participants defines her own dimensions.
#'
#' **The canonical representation of open sorting data** is therefore a *list* of matrices, one for each participant.
#' Every *individual* matrix is a [psOpenSort()] object, and together, they form a [psOpenSorts()] list.
#' The rows in these matrices are the items, the columns are the dimensions, and cells are the assignment.
#' Optional dimension descriptions are included as attributes of the matrices.
#'
#' @param open_sorts
#' A list of matrices, one for each participant.
#' Matrices must be [psOpenSort()] objects, or coercable via [as_psOpenSort()].
#'
#' @example tests/testthat/helper_psOpenSort.R
#' @example tests/testthat/helper_psOpenSorts.R
#'
#' @return Object of class `psOpenSorts`.
#'
#' @template construction_helpers
#'
#' @export
psOpenSorts <- function(open_sorts) {
  open_sorts <- lapply(X = open_sorts, FUN = function(x) as_psOpenSort(x))

  # for no particular reason, we make the first in the list the benchmark
  which_cl <- as.logical(inherits(x = open_sorts[[1]], what = os_subcl$class_singular, which = TRUE))
  subclass <- os_subcl[which_cl, "class_plural"]

  validate_psOpenSorts(new_psOpenSorts(open_sorts = open_sorts, subclass = subclass))
}

# constructor
new_psOpenSorts <- function(open_sorts, subclass = NULL) {
  structure(
    .Data = open_sorts,
    class = c(subclass, "psOpenSorts")
  )
}

# validator
validate_psOpenSorts <- function(open_sorts) {
  assert_list(x = open_sorts,
              any.missing = TRUE,
              all.missing = FALSE,
              types = "matrix")

  # for no particular reason, we make the first in the list the benchmark
  n_items <- nrow(open_sorts[[1]])
  item_handles <- rownames(open_sorts[[1]])
  which_cl <- as.logical(inherits(x = open_sorts[[1]], what = os_subcl$class_singular, which = TRUE))
  mode <- os_subcl[which_cl, "mode"]

  # TODO a c or list method might be a better approach than these ugly
  lapply(X = open_sorts, FUN = function(x) {
    validate_psOpenSort(osort = x)
    assert_matrix(x = x,
                  mode = mode,
                  nrows = n_items)
    assert_set_equal(x = rownames(x),
                     y = item_handles,
                     ordered = TRUE)
  })
  return(open_sorts)
}

# coercion ====
#' @rdname psOpenSorts
#'
#' @param logical_open_sorts a character matrix with rows as items, columns as participants and  **logical category assignments** as character strings in cells.
#' Categories are identified by a subset from `LETTERS`, same as in `descriptions_messy`.
#' Assignments must be the same subset of `LETTERS` as the column names in `descriptions_messy`.
#' Rows and columns must be named.
#'
#' For example, if some participant assigned her (self-described) categories `A`, `D` and `Z` to some item, the cell for that item and participant would read `"A, D, Z"`.
#' Order and punctuation are ignored.
#'
#' See `note`.
#'
#' @param ... further arguments passed to or from other methods.
#'
#' @export
as_psLogicalOpenSorts <- function(logical_open_sorts, ...) {
  UseMethod(generic = "as_psLogicalOpenSorts")
}

#' @export
as_psLogicalOpenSorts.default <- function(logical_open_sorts, ...) {
  stop_coercion(x = logical_open_sorts, class = "psLogicalOpenSorts")
}

#' @export
as_psLogicalOpenSorts.psLogicalOpenSorts <- function(logical_open_sorts, ...) {
  validate_psOpenSorts(open_sorts = logical_open_sorts)
}

#' @describeIn psOpenSorts
#'
#' @param descriptions_messy a character matrix with rows as category indices, columns as participants and **category descriptions** in cells.
#' Rows *must* be named by a subset of `LETTERS` to conveniently enter, and identify them from `logical_open_sort`.
#' The row names are arbitrary identifiers, but will be retained for the canonical form.
#' Columns *must* be named as participants.
#'
#' Defaults to `NULL`, in which case no descriptions are available.
#'
#' Notice category description in one row have *nothing in common* other than their *indices*:
#' For example, the category descriptions in a row named `'B'` are all by different participants, and may refer to entirely different aspects.
#' They are only conveniently entered in a table, and all share the fact that they were the *second* description provided.
#'
#' When some category has not been defined by the participant, the value in the cell should be `NA`.
#' Empty strings `""` will also be considered `NA`.
#'
#' @param keep_LETTERS a logical flag.
#' Defaults to `TRUE`, in which case the `LETTERS` for the category descriptions and assignments are retained as names, even though they are just indices and not actual meaningful names (useful for debugging).
#'
#' @details
#' The canonical representation of open sorts in [psOpenSorts()] can be cumbersome to enter manually.
#' For *logical* (nominally-scaled) open sorts, a simpler, but messier format can be conveniently entered as two separate spreadsheets of `descriptions_messy` and `logical_open_sorts` using [as_psLogicalOpenSorts()].
#'
#' @note
#' When a category is assigned, but never described, it is `TRUE` in the respective logical matrix entries and their description is `NA`:
#' This is still considered valuable, if incomplete information.
#' When a category is described, but never assigned, it is omitted from the data entirely.
#'
#' When *no* category was assigned to some item in `logical_open_sorts`, an empty character string `""` should be in the respective cell.
#'
#' An `NA` value implies that the given participant never considered the given items *at all*, across *all* her categories.
#' Notice this implies *limited scenarios of `NA`* for data entered in this messy, convenient form.
#' The more complicated cases, where a participant did consider *some*, but *not all* items in the assignment of a category, or -- equivalently -- all categories in their assessment of all items, cannot be recorded in this convenience format.
#' Such more granular `NA` records can, however, be recorded in the canonical data representation, where the respective cell of the items x category logical matrix would be `NA`.
#' If your data gathering procedure produces such granular `NA` records, do not use this convenience function.
#'
#' @export
as_psLogicalOpenSorts.matrix <- function(logical_open_sorts,
                                         descriptions_messy = NULL,
                                         keep_LETTERS = TRUE,
                                         ...) {
  # variable names are too long
  ass <- logical_open_sorts
  desc <- descriptions_messy

  # Input validation ====
  assert_matrix(x = ass,
                mode = "character",
                any.missing = TRUE,
                all.missing = FALSE,
                row.names = "strict",
                col.names = "strict",
                null.ok = FALSE)

  if (!is.null(desc)) {
    desc[desc == ""] <- NA  # empty strings are considered NAs
    assert_matrix(x = desc,
                  mode = "character",
                  any.missing = TRUE,
                  all.missing = FALSE,
                  null.ok = FALSE,
                  row.names = "strict",
                  col.names = "strict")
    check_subset(x = rownames(desc),
                 choices = LETTERS,
                 empty.ok = FALSE)
    assert_set_equal(x = colnames(desc), y = colnames(ass), ordered = TRUE)
  }

  assert_flag(x = keep_LETTERS)

  # body ====
  # create empty object
  cat_canon <- sapply(X = colnames(ass), FUN = function(x) NULL)

  for (p in names(cat_canon)) {
    # we take *all* categories, either assigned OR defined
    max_cats <- unique(c(
      LETTERS[LETTERS %in% unlist(strsplit(x = ass[, p], split = ""))],
      names(desc[,p][!is.na(desc[,p])])
    ))
    max_cats <- max_cats[order(max_cats)]  # just in case, this makes results nicer to cross-check

    # now we can create the logical matrix of appropriate rank
    m <- matrix(data = NA,
                nrow = nrow(ass),
                ncol = length(max_cats),
                dimnames = list(items = rownames(ass), categories = max_cats))

    catsplit <- strsplit(x = ass[, p],
                         split = "")

    for (i in rownames(m)) {
      if (anyNA(catsplit[[i]])) {
        m[i, ] <- NA  # these are the items that participant never saw
      } else {
        m[i, ] <- max_cats %in% catsplit[[i]]
      }
    }
    better_desc <- desc[, p]  # these are the descriptions of current persons
    names(better_desc) <- rownames(desc)
    final_desc <- better_desc[max_cats]

    if (keep_LETTERS) {
      # let's retain the simple LETTERS, even if they are meaningless, they help with debugging at least
    } else {
      # we can actually always kill them, because as per the convenient input format, they are always meaningless
      colnames(m) <- NULL
      names(final_desc) <- NULL
    }

    cat_canon[[p]] <- as_psOpenSort(osort = m, descriptions = final_desc)
  }
  cat_canon <- psOpenSorts(open_sorts = cat_canon)
  return(cat_canon)
}

#' @describeIn psOpenSorts coerce messy, but convenient data.frame or matrix with *all* sorts to psLogicalOpenSorts
#' @export
as_psLogicalOpenSorts.data.frame <- as_psLogicalOpenSorts.matrix

# stupid helper just to make messy format out of clean
make_messy <- function(open_sorts) {
  ass <- sapply(X = open_sorts, FUN = function(part) {
    apply(X = part, MARGIN = 1, FUN = function(x) {
      paste(LETTERS[1:length(x)][x], collapse = ", ")
    })
  })

  maxlength <- max(sapply(X = open_sorts, function(x) ncol(x)))
  l <- sapply(X = open_sorts, simplify = FALSE, FUN = function(x) {
    vec <- unlist(attr(x = x, which = "descriptions"))
    if (is.null(vec)) {
      vec <- NA
    }
    length(vec) <- maxlength
    vec
    return(vec)
  })
  desc <- do.call(what = cbind, args = l)
  rownames(desc) <- LETTERS[1:nrow(desc)]

  return(list(ass = ass, desc = desc))
}

# plotting ====
#' @describeIn psOpenSorts *Summarize* list of open sorts
#'
#' @param x a [psOpenSorts], created by [psOpenSorts()].
#'
#' @export
tidy.psLogicalOpenSorts <- function(x) {
  by_person <- sapply(X = x, FUN = function(x) unlist(summary.psLogicalOpenSort(x)[1:3]), simplify = TRUE, USE.NAMES = FALSE)
  by_person <- as.data.frame(t(by_person))
  by_person$name <- rownames(by_person)

  # below two are dicey, because n of cat and n of t is different, so these are unweighted sums
  by_both <- sapply(X = x, FUN = function(x) summary.psLogicalOpenSort(x)$n_true_by_item)
  by_item <- rowSums(x = by_both, na.rm = TRUE)

  return(by_person)
}

#' @describeIn psOpenSorts plots Summary
#'
#' @param object a [psLogicalOpenSorts], created by [psOpenSorts()].
#'
#' @examples
#' ggplot2::autoplot(object = los)
#'
#' @export
autoplot.psOpenSorts <- function(object) {
  df <- tidy.psLogicalOpenSorts(x = object)

  g <- ggplot(data = df,
              mapping = aes_string(x = 'n_dim', y = 'true_per_dim', label = 'name'))
  g <- g + geom_smooth(method = "lm", show.legend = TRUE)
  g <- g + geom_point()
  g <- g + xlab("Number of Dimensions (Categories)")
  g <- g + ylab("Number of Assignments (TRUEs) per Dimension (Category)")
  g <- g + xlim(0, NA)
  g <- g + ylim(0, NA)
  g <- g + scale_x_continuous(breaks = c(1:max(df$n_dim)))

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    # repel labels
    g <- g + ggrepel::geom_label_repel()
  } else {
    warning("Package 'ggrepel' is not installed, labels might overplot.")
  }
  g
}


#' @title Create Co-Occurence Matrices.
#'
#' @export
#'
#' @description Creates co-occurence matrices from logical q-category assignments.
#'
#' @param ass Named list of logical matrices, one for each participant.
#' Each logical matrix has items as named rows, category indices as columns and logical values in cells.
#'
#' @return
#' An integer array with items as rows and columns, participants as third dimension and cells as co-occurence counts.
#'
#' @details
#' The diagonal is replaced with the *maximum number of categories* for that person, to standardize the entire table.
#'
#' @family import
#'
#' @author Maximilian Held
#'
count_cooccur <- function(ass) {

  # input validation ===
  expect_list(x = ass,
              types = "matrix",
              all.missing = FALSE)
  for (i in names(ass)) {
    expect_matrix(x = ass[[i]],
                  mode = "logical",
                  any.missing = TRUE,
                  all.missing = FALSE,
                  row.names = "unique",
                  null.ok = FALSE,
                  info = paste("Matrix", i, "is not as expected."))
  }

  # body ===
  a <- sapply(X = ass, USE.NAMES = TRUE, simplify = "array", FUN = function(x) {
    m <- tcrossprod(x)
    storage.mode(m) <- "integer"
    diag(m) <- ncol(x)
    return(m)
  })
  names(dimnames(a))[3] <- "people"
  return(a)
}
