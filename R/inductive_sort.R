#' @title Construct list of categorical assignments
#'
#' @export
#'
#' @param assignments
#' a named list of matrices, with names as a participant names.
#' Matrices have identical item-handles as row names, and may have arbitrary (ignored) column names.
#' Matrices must all be of one data type, either
#' - `logical` for *nominal*-scaled sorts, where a category applies or does not apply,
#' - `integer` for *ordinally*-scaled sorts, where a category applies to some item *more or less* than to another other item,
#' - `numeric` for *interval* or *ratio*-scaled sorts, where a category applies to some item *by some amount more or less* than to another item.
#' Notice that -- counterintuitively -- *categorically*-scaled sorts are not allowed.
#'
#' @template construction_helpers
#'
#' @examples
#' lisa <- matrix(data = c(TRUE, FALSE, FALSE, TRUE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("foo", "bar")))
#' peter <- matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("foo", "bar")))
#' assignments <- list(lisa = lisa, peter = peter)
#' assignments <- pensieveAssignments(list(lisa = lisa, peter = peter))
pensieveAssignments <- function(assignments) {
  validate_pensieveAssignments(new_pensieveAssignments(assignments = assignments))
}

# constructor
new_pensieveAssignments <- function(assignments) {
  structure(
    .Data = assignments,
    class = c("pensieveAssignments")
  )
}

# validator
validate_pensieveAssignments <- function(assignments) {
  assert_list(x = assignments,
              any.missing = FALSE,
              all.missing = FALSE,
              names = "strict")
  data_type <- mode(assignments[[1]])  # we'll just take the first
  n_items <- nrow(assignments[[1]])
  item_handles <- rownames(assignments[[1]])
  assert_choice(x = data_type, choices = c("logical", "integer", "numeric"))
  lapply(X = assignments, FUN = function(x) {
    assert_matrix(x = x,
                  mode = data_type,
                  any.missing = TRUE,
                  all.missing = TRUE,
                  nrows = n_items,
                  row.names = "strict")
    assert_set_equal(x = rownames(x),
                     y = item_handles,
                     ordered = TRUE)
  })
  return(assignments)
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


# import helper

#' @title Import Q categorisation data.
#'
#' @export
#'
#' @description Imports category descriptions and assignments from convenient, but messy format.
#'
#' @param desc A character matrix with columns as participants, rows as categories, and **category descriptions** in cells.
#' Rows and columns be named.
#' See `details`.
#'
#' @param ass A character matrix with columns as participants, rows as items, and category assignments as character vectors of length 1 in cells.
#' Assignments must be the same subset of `LETTERS` as the column names in `desc`.
#' Rows and columns must be named.
#' See `details`.
#'
#' @details
#' The open-ended categorizations in the Q-cat procedure *cannot* be compared between participants, because each participants defines her own categories.
#' **The canonical representation of Q-cat data** is therefore a *list* of logical matrices, one for each participant.
#' The rows in these logical matrices are the items, the columns are the category, and cells are either `TRUE`, `FALSE` or `NA` *indeces* for that participant
#' Crucially, because the categories cannot be compared between participants, the logical matrices must not have column names.
#' The logical category assignments are linked to the respective category descriptions by their indeces only.
#'
#' This canonical representation is not very convenient for **data entry**.
#' This function transforms conveniently entered, but messy Q-cat data into their canonical representation.
#'
#' Q-cat data are easily entered as two separate spreadsheets.
#'
#' - **Category Descriptions.**
#'   (`desc`) A character matrix of open-ended category descriptions.
#'   Notice category description in one row have *nothing in common* other than their *indices*:
#'   For example, the category descriptions in a row named `'B'` are all by different participants, and may refer to entirely different aspects.
#'   They are only conveniently entered in a table, and all share the fact that they were the *second* description provided.
#'
#'   When some category identifier has not been used, the value in the cell should be `NA`.
#'
#'   Rows *must* be named by a subset of `LETTERS` to conveniently enter, and identify them from `ass`.
#'   The row names are arbitrary identifiers, and will be removed from the canonical form.
#'
#'   Columns *must* be named as participants.
#'
#' - **Category Assignments**
#'   (`ass`) A character matrix of category assignments.
#'   Each cell includes a character string with the category identifiers which that participant assigned to the given item.
#'   Categories are identified by a subset from `LETTERS`, same as in `desc`.
#'   For example, if some participant assigned her (self-described) categories `A`, `D` and `Z` to some item, the cell for that item and participant would read `"A, D, Z"`.
#'   Order and punctuation are ignored.
#'
#'   When *no* category was assigned to some item, an empty character string should be in the cell `""`.
#'   See `note`.
#'
#' When categories are described, but never assigned, they are `FALSE` on all items in the logical matrix.
#' When categories are assigned, but never described, they are `TRUE` in the respective logical matrix entries and their description is `NA`.
#' See `note` for more granular treatment of `NA`s.
#'
#'
#' @note
#' Notice that when entered in this convenient way, *there can only be a limited set of `NA` values*.
#' `NA`s can only be recorded when some participants never saw a set of items *over all* their categories, in which case the entire cell will be `NA`.
#' The more complicated cases, where a participant did not consider all items in the assignment of a category, or -- equivalently -- all categories in their assessment of all items cannot be recorded in this convenience format.
#' Such more granular `NA` records can, however, be recorded in the canonical data representation, where the respective cell of the items x category logical matrix would be `NA`.
#' If your data gathering procedure produces such granular `NA` records, do not use this convenience function.
#'
#' @return
#' A list of logical matrices, one for each participant.
#' Each matrix has items as rows, category indices as columns and logical values as cells.
#' `TRUE` if a category was assigned to an item, `FALSE` if not and `NA` if the combination was not considered by the participant.
#'
#' @family import
#'
#' @author Maximilian Held
#'
#'
import_qcat <- function(desc, ass) {

  # Input validation ====
  expect_matrix(x = desc,
                mode = "character",
                any.missing = TRUE,
                all.missing = FALSE,
                row.names = "strict",
                col.names = "strict")
  check_subset(x = rownames(desc),
               choices = LETTERS,
               empty.ok = FALSE)

  expect_matrix(x = ass,
                mode = "character",
                any.missing = TRUE,
                all.missing = FALSE,
                #row.names = "strict",
                col.names = "strict",
                ncols = ncol(desc))

  # body ====

  # create empty object
  cat_canon <- sapply(X = colnames(ass), FUN = function(x) NULL)

  for (p in names(cat_canon)) {
    described_cats <- rownames(desc)[!is.na(desc[, p])]
    used_cats <- LETTERS[LETTERS %in% unlist(strsplit(x = ass[, p], split = ""))]

    max_cats <- unique(described_cats, used_cats)  # these are the maximum number of categories used.
    # remember that it is possible that a category is described, but not assigned and vice versa.
    # See note in docs.

    # now we can create the logical matrix of appropriate rank
    m <- matrix(data = NA,
                nrow = nrow(ass),
                ncol = length(max_cats),
                dimnames = list(items = rownames(ass), categories = NULL))

    catsplit <- strsplit(x = ass[, p],
                         split = "")

    for (i in rownames(m)) {
      if (anyNA(catsplit[[i]])) {
        m[i, ] <- NA  # these are the items that participant never saw
      } else {
        m[i, ] <- max_cats %in% catsplit[[i]]
      }
    }
    cat_canon[[p]] <- m
  }
  return(cat_canon)
}

