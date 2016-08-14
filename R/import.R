#' @title Import Q categorisation data.
#'
#' @export
#'
#' @description Imports category descriptions and assignments from convenient, but messy format.
#'
#' @param desc A character matrix with columns as participants, rows as categories, and \strong{category descriptions} in cells.
#' Rows and columns be named.
#' See `details`.
#'
#' @param ass A character matrix with columns as participants, rows as items, and category assignments as character vectors of length 1 in cells.
#' Assignments must be the same subset of \code{LETTERS} as the column names in \code{desc}.
#' Rows and columns must be named.
#' See `details`.
#'
#' @details
#' The open-ended categorizations in the Q-cat procedure \emph{cannot} be compared between participants, because each participants defines her own categories.
#' \strong{The canonical representation of Q-cat data} is therefore a \emph{list} of logical matrices, one for each participant.
#' The rows in these logical matrices are the items, the columns are the category, and cells are either \code{TRUE}, \code{FALSE} or \code{NA}. \emph{indices} for that participant
#' Crucially, because the categories cannot be compared between participants, the logical matrices must not have column names.
#' The logical category assignments are linked to the respective category descriptions by their indeces only.
#'
#' This canonical representation is not very convenient for \strong{data entry}.
#' This function transforms conveniently entered, but messy Q-cat data into their canonical representation.
#'
#' Q-cat data are easily entered as two separate spreadsheets.
#' \describe{
#'  \item{Category Descriptions.}{
#'    (\code{desc}) A character matrix of open-ended category descriptions.
#'    Notice category description in one row have \emph{nothing in common} other than their \emph{indices}:
#'    For example, the category descriptions in a row named \code{"B"} are all by different participants, and may refer to entirely different aspects.
#'    They are only conveniently entered in a table, and all share the fact that they were the \emph{second} description provided.
#'
#'    When some category identifier has not been used, the value in the cell should be \code{NA}.
#'
#'    Rows \emph{must} be named by a subset of \code{LETTERS} to conveniently enter, and identify them from \code{ass}.
#'    The row names are arbitrary identifiers, and will be removed from the canonical form.
#'
#'    Columns \emph{must} be named as participants.
#'  }
#'  \item{Category Assignments.}{
#'    (\code{ass}) A character matrix of category assignments.
#'    Each cell includes a character string with the category identifiers which that participant assigned to the given item.
#'    Categories are identified by a subset from \code{LETTERS}, same as in \code{desc}.
#'    For example, if some participant assigned her (self-described) categories `A`, `D` and `Z` to some item, the cell for that item and participant would read \code{"A, D, Z"}.
#'    Order and punctuation are ignored.
#'
#'    When \emph{no} category was assigned to some item, an empty character string should be in the cell \code{""}.
#'    See `note`.
#'  }
#' }
#'
#' When categories are described, but never assigned, they are \code{FALSE} on all items in the logical matrix.
#' When categories are assigned, but never described, they are \code{TRUE} in the respective logical matrix entries and their description is \code{NA}.
#' See `note` for more granular treatment of \code{NA}s.
#'
#'
#' @note
#' Notice that when entered in this convenient way, \emph{there can only be a limited set of \code{NA} values}.
#' \code{NA}s can only be recorded when some participants never saw a set of items \emph{over all} their categories, in which case the entire cell will be \code{NA}.
#' The more complicated cases, where a participant did not consider all items in the assignment of a category, or -- equivalently -- all categories in their assessment of all items cannot be recorded in this convenience format.
#' Such more granular \code{NA} records can, however, be recorded in the canonical data representation, where the respective cell of the items x category logical matrix would be \code{NA}.
#' If your data gathering procedure produces such granular \code{NA} records, do not use this convenience function.
#'
#' @return
#' A list of logical matrices, one for each participant.
#' Each matrix has items as rows, category indices as columns and logical values as cells.
#' \code{TRUE} if a category was assigned to an item, \code{FALSE} if not and \code{NA} if the combination was not considered by the participant.
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

