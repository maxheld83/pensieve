#' @title Construct *single* and *multiple* open sort matrix.
#'
#' @export
#'
#' @template construction_helpers
#'
#' @examples
#' # Lisas open sort, matching by index
#' assignments <- matrix(data = c(TRUE, FALSE, FALSE, TRUE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("cat", "dog")))
#' descriptions <- c("a pet which largely takes care of itself",
#'                   "is known to have saved humans")
#' lisa <- pensieveOpenSort(assignments = assignments, descriptions = descriptions)
#'
#' # Peters open sort, matching by name
#' assignments <- matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("cat", "dog"),
#'                                       categories = c("in_homes",
#'                                                      "quiet",
#'                                                      "herbivore")))
#' descriptions <- c(in_homes = "Animal found in peoples homes.",
#'                   quiet = "Does not make a lot of noise.",
#'                   herbivore = "Eats plants.")
#' peter <- pensieveOpenSort(assignments = assignments, descriptions = descriptions)
#'
#' # Rebeccas open sort, without any descriptions provided
#' assignments <- matrix(data = c(FALSE, FALSE, TRUE, TRUE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("cat", "dog")))
#' rebecca <- pensieveOpenSort(assignments = assignments, descriptions = NULL)
#' # providing no description is possible, but makes interpretation hard, if not meaningless.
#'
#' # now let's combine the individual sort into a list
#' open_sorts <- pensieveOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))
#'
#' @name pensieveOpenSorts
NULL

#' @describeIn pensieveOpenSorts Creates *individual* open sort.
#'
#' @param assignments
#' a matrix with item-handles as row names, arbitrary or empty column names, and open sort value in cells.
#' Matrix must be either
#' - `logical` for *nominal*-scaled sort, where an open category applies (`TRUE`) or does not apply (`FALSE`),
#' - `integer` for *ordinally*-scaled sort, where an open category applies to some item *more* (`2nd` rank) *or less* (`3rd` rank) than to another other item,
#' - `numeric` for *interval* or *ratio*-scaled sort, where an open category applies to some item *by some amount more or less* (say `2.4` units) than to another item.
#' Notice that -- counterintuitively -- *categorically*-scaled open sorts are not allowed.
#' If columns are named, they must be the same as the names in `descriptions`.
#' Either way, `assignments` and `descriptions` are always *matched by index only*: the first column from `assignments`, must be the first element of `description`, and so forth.
#'
#' @param descriptions
#' a character vector giving the open-ended category description provided by the participant.
#' Can be named.
#' Defaults to `NULL`, in which case the user-defined categories are unknown (not recommended).
#'
#' @export
pensieveOpenSort <- function(assignments, descriptions = NULL) {

  if (!is.null(descriptions)) {
    # prepare descriptions; must always be named LIST
    if (is.null(names(descriptions))) {
      names(descriptions) <- as.character(1:length(descriptions))
    }
    descriptions <- as.list(descriptions)
  }

  validate_pensieveOpenSort(new_pensieveOpenSort(assignments = assignments, descriptions = descriptions))
}

# constructor
new_pensieveOpenSort <- function(assignments, descriptions) {
  # remember that matching is ALWAYS by index only, the rest is fluff
  do.call(what = structure, args = append(
    x = list(.Data = assignments,
             class = c("pensieveOpenSort", "Matrix")),
    values = descriptions))
}

# validator
validate_pensieveOpenSort <- function(assignments) {

  # validate assignments
  assert_matrix(x = assignments,
                row.names = "strict",
                null.ok = FALSE)

  descriptions <- attributes(assignments)
  descriptions <- descriptions[!(names(descriptions) %in% c("dim", "dimnames", "class"))]
  if (length(descriptions) == 0) {  # recreate NULL assignment, when there are none in attr
    descriptions <- NULL
  }

  if (!is.null(descriptions)) {
    # validate descriptions
    assert_list(x = descriptions,
                types = "character",
                any.missing = TRUE,
                all.missing = TRUE,
                unique = TRUE,
                names = "unique", # strict fails on "1" etc
                null.ok = FALSE,
                len = ncol(assignments)) # this already validates against assignments

    if (!is.null(colnames(assignments))) {
      # validate descriptions AND assignments
      assert_names(x = colnames(assignments),
                   type = "unique")

      assert_set_equal(x = names(descriptions),
                       y = colnames(assignments),
                       ordered = TRUE)
    }
  }
  return(assignments)
}

#' @describeIn pensieveOpenSorts *Combine* individual open sorts in a list.
#'
#' @param open_sorts named list of matrices created by [pensieveOpenSort()], one for each participant.
#' Must all be of equal data type and all have the same rows and rownames.
pensieveOpenSorts <- function(open_sorts) {
  validate_pensieveOpenSorts(new_pensieveOpenSorts(open_sorts = open_sorts))
}

# constructor
new_pensieveOpenSorts <- function(open_sorts) {
  structure(
    .Data = open_sorts,
    class = c("pensieveOpenSorts")
  )
}

# validator
validate_pensieveOpenSorts <- function(open_sorts) {
  assert_list(x = open_sorts,
              any.missing = TRUE,
              all.missing = TRUE,
              names = "strict",
              types = "matrix")

  # for no particular reason, we make the first in the list the benchmark
  data_type <- mode(open_sorts[[1]])
  n_items <- nrow(open_sorts[[1]])
  item_handles <- rownames(open_sorts[[1]])

  assert_choice(x = data_type, choices = c("logical", "integer", "numeric"))
  lapply(X = open_sorts, FUN = function(x) {
    validate_pensieveOpenSort(assignments = x)
    assert_matrix(x = x,
                  mode = data_type,
                  nrows = n_items,
                  row.names = "strict")
    assert_set_equal(x = rownames(x),
                     y = item_handles,
                     ordered = TRUE)
  })
  return(open_sorts)
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
