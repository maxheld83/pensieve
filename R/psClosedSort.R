# TODO build psClosedSort class (SINGULAR!)
# this is a matrix of items x sorting dimensions (horiz, vertical)
# it is always stored internally in such a TIDY form
# the below untidy/wide form (as it exists on the table) is only ever imported or rendered at the edges of pensieve
# each of the psClosedSort matrices (each one is ONE participant) may have DIVERGENT properties such as forced/free distro or brick wall binning
# if such inconsistent psClosedSort exist (in a LIST probably), there will be a method to ABIND them into the PLURAL psClosedSortS, and this method takes care of standardizing across the divergent properties (Such as z-scoring), if at all possible

# TODO just for testing, delete those
# gh_input_helper <- readr::read_csv(file = "../groundhog/input_helper.csv")
# items <- gh_input_helper$handle
# csort <- grid
# csort[,] <- NA
# mode(csort) <- "character"
# row <- as.integer(7)
# column <- as.integer(2)
# item <- "party"

#' @title Place item into row and column of a closed sort.
#' @param csort A matrix of class (or coercable to) `psClosedSort`, in *wide* form, with rows as ties, columns as ranks and item handles in cells.
#' @param row An integer scalar giving the row index.
#' @param column An integer scalar giving the column index.
#' @param item A character string giving the item handle.
#' Defaults to `NA`, in which case `row` and `column` cell is set to `NA`.
#' Useful for *removing* items.
#' @param grid A logical matrix of class (or coercable to) `psGrid`.
#' Defaults to `NULL`, in which case all cells in `csort` are allowed as `TRUE`.
#' Useful for *free* distributions.
#' @param items A character vector giving *possible* item handles.
#' @return A matrix of class `psClosedSort`.
#' @noRd
append_psClosedSort <- function(csort, row, column, item = NA, grid = NULL, items) {
  # input validation
  assert_matrix(
    x = csort,
    mode = "character",
    any.missing = TRUE,
    all.missing = TRUE,
    null.ok = FALSE)
  # TODO in future, function should allow NULL rows, because rows are actually meaningless, and some upstream uses may not have row info, such as when data is only in long form already
  assert_scalar(x = row, na.ok = FALSE, null.ok = FALSE)
  assert_scalar(x = column, na.ok = FALSE, null.ok = FALSE)
  assert_string(x = item, na.ok = TRUE, null.ok = FALSE)
  assert_matrix(x = grid, mode = "logical", any.missing = FALSE, null.ok = TRUE)
  assert_character(x = items, any.missing = FALSE, unique = TRUE, null.ok = FALSE)

  # prepare values
  item <- as.character(item)
  if (is.null(grid)) {
    grid <- matrix(data = TRUE, nrow = nrow(csort), ncol = ncol(csort))
  }

  # TODO some of this might be better tested in wrappers; would be called too often in here
  # consistency checks
  assert_matrix(
    x = csort,
    nrows = nrow(grid),
    ncols = ncol(grid)
  )
  assert_integer(x = row, lower = 0, upper = nrow(csort))
  assert_integer(x = column, lower = 0, upper = ncol(csort))
  assert_choice(x = item, choices = c(items, NA))
  assert_character(x = items, max.len = sum(grid))

  if (!is.na(item)) {
    # item must not already be placed in csort
    assert_false(x = item %in% csort, na.ok = TRUE)
  }
  assert_true(x = grid[row, column], na.ok = FALSE)

  csort[row, column] <- item
  return(csort)
}
