#' @title Construct *single* and *multiple* open sort matrix.
#'
#' @export
#'
#' @template construction_helpers
#'
#' @details
#' Open sorting categorizations *cannot* be compared between participants, because each participants defines her own categories.
#' **The canonical representation of open sorting data** is therefore a *list* of matrices, one for each participant.
#' Every *individual* matrix is a [psOpenSort()] object, and together, they form a [psOpenSorts()] list.
#' The rows in these matrices are the items, the columns are the category, and cells are the assignment.
#'
#' @examples
#' # Lisas open sort, matching by index
#' assignments <- matrix(data = c(TRUE, FALSE, FALSE, TRUE),
#'                       nrow = 2,
#'                       dimnames = list(items = c("cat", "dog")))
#' descriptions <- c("a pet which largely takes care of itself",
#'                   NA)
#' lisa <- psOpenSort(assignments = assignments, descriptions = descriptions)
#'
#' # Peters open sort, matching by name
#' assignments <- matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
#'                       nrow = 2,
#'                       dimnames = list(items = c("cat", "dog"),
#'                                       categories = c("in_homes",
#'                                                      "quiet",
#'                                                      "herbivore")))
#' descriptions <- c(in_homes = "Animal found in peoples homes.",
#'                   quiet = "Does not make a lot of noise.",
#'                   herbivore = "Eats plants.")
#' peter <- psOpenSort(assignments = assignments, descriptions = descriptions)
#'
#' # Rebeccas open sort, without any descriptions provided
#' assignments <- matrix(data = c(FALSE, FALSE, TRUE, TRUE),
#'                       nrow = 2,
#'                       dimnames = list(handles = c("cat", "dog")))
#' rebecca <- psOpenSort(assignments = assignments, descriptions = NULL)
#' # providing no description is possible, but makes interpretation hard, if not meaningless.
#'
#' # now let's combine the individual sort into a list
#' open_sorts <- psOpenSorts(open_sorts = list(lisa = lisa, peter = peter, rebecca = rebecca))
#'
#' @name psOpenSorts
NULL

#' @describeIn psOpenSorts Creates *individual* open sort.
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
psOpenSort <- function(assignments, descriptions = NULL) {
  if (!is.null(descriptions)) {
    # prepare descriptions; must always be named LIST
    if (is.null(names(descriptions))) {
      names(descriptions) <- as.character(1:length(descriptions))
    }
    descriptions <- as.list(descriptions)
  }

  validate_psOpenSort(new_psOpenSort(assignments = assignments, descriptions = descriptions))
}

# constructor
new_psOpenSort <- function(assignments, descriptions) {
  # remember that matching is ALWAYS by index only, the rest is fluff

  # must protect against NO colnames or rownames, make "fake" ones out of integers, because all of the downstream expects colnames
  # also, description attributes must always be indexed by character, so this makes that easier too
  if (is.null(colnames(assignments))) {
    safe_colnames <- as.character(1:ncol(assignments))
  } else {
    safe_colnames <- colnames(assignments)
  }
  if (is.null(rownames(assignments))) {
    safe_rownames <- as.character(1:nrow(assignments))
  } else {
    safe_rownames <- rownames(assignments)
  }

  do.call(what = structure, args = append(
    x = list(.Data = assignments,
             dimnames = list(items = safe_rownames, categories = safe_colnames),
             class = c("psOpenSort", "matrix")),
    values = list(descriptions = descriptions)))
}

# validator
validate_psOpenSort <- function(assignments) {

  # validate assignments
  assert_matrix(x = assignments,
                row.names = "strict",
                null.ok = FALSE)
  assert_set_equal(x = names(dimnames(assignments)), y = c("items", "categories"))

  descriptions <- attributes(assignments)$descriptions
  if (length(descriptions) == 0) {  # recreate NULL assignment, when there are none in attr
    descriptions <- NULL
  }

  if (!is.null(descriptions)) {
    # validate descriptions
    assert_list(x = descriptions,
                types = "character",
                any.missing = TRUE,
                all.missing = TRUE,
                unique = FALSE,  # oddly, duplicate NAs count as non-unique, hence extra test below
                names = "unique", # strict fails on "1" etc
                null.ok = FALSE,
                len = ncol(assignments)) # this already validates against assignments

    # must test if non-NAs are at least unique
    assert_list(x = descriptions[!(is.na(descriptions))],
                unique = TRUE)

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

#' @rdname psOpenSorts
# #' @describeIn psOpenSorts Prepare *individual* open sort for bipartite plotting.
#'
#' @param x a [psOpenSort], created by [psOpenSort()].
#'
#' @export
tidy.psOpenSort <- function(x) {
  # input validation ====
  assert_class(x = x, classes = "psOpenSort", null.ok = FALSE)
  #TODO replace this with a real asserter, once available
  invisible(validate_psOpenSort(assignments = x))

  # prep EDGE data ====
  # melt the edges
  edge_df <- reshape2::melt(data = x, as.is = TRUE)
  edge_df[[2]] <- as.character(edge_df[[2]])
  edge_df <- edge_df[!(is.na(edge_df$value)), ]  # kill NAs
  edge_df <- edge_df[edge_df$value, ]  # take only TRUEs
  edge_df <- edge_df[, c("items", "categories")]

  # NAs as nodes don't work, so we have to kill these
  edge_df <- edge_df[!(is.na(edge_df$categories) | is.na(edge_df$items)), ]
  # this implies that missing sub- and supercodes must LATER be added again via the ggplot scale_color function, so that the legend is always complete and color schemes are comparable.

  # prep NODE VERTICES DATA ====
  fulldesc <- unlist(attributes(x)$descriptions[colnames(x)])
  if (is.null(fulldesc)) {
    node_df <- data.frame(name = c(colnames(x), rownames(x)),
                          type = c(rep(FALSE, ncol(x)), rep(TRUE, nrow(x))),
                          labels = c(rep(NA, ncol(x)), rownames(x)),
                          stringsAsFactors = FALSE)
  } else {
    node_df <- data.frame(name = c(colnames(x), rownames(x)),
                          type = c(rep(FALSE, ncol(x)), rep(TRUE, nrow(x))),
                          labels = c(fulldesc, rownames(x)),
                          stringsAsFactors = FALSE)
  }

  return(list(edge_df = edge_df, node_df = node_df))
}


#' @rdname psOpenSorts
# #' @describeIn psOpenSorts Create bipartite plot from *individual* open sort.
#'
#' @param object a [psOpenSort], created by [psOpenSort()].
#'
#' @param edge_codings a tibble with category description indeces in the first column, and arbitrary metadata *about the descriptions* in limited later columns:
#' - 2nd column will be mapped to line color,
#' - 3rd column will be mapped to line type (and must be discrete).
#' Later columns will be ignored.
#' Useful if participants or researchers have coded the open-ended descriptions in some way.
#' Category description indeces must be a subset of the column names in `x`.
#' If more then one code applies to a category, multiple rows with identical categories can exist, and multiple, "fanned-out" edges will be drawn between the respective nodes.
#' See note.
#' Defaults to `NULL`.
#'
#' @param str_wrap_width integer scalar, giving the maximum number of characters after which to insert a newline, passed on to [stringr::str_wrap()].
#' Defaults to `30`.
#' Useful for long descriptions.
#'
#' @note
#' To render the resulting `ggplot` object, you must *manually* call `library(ggraph)` somewhere in your script (as per [this limitation](https://github.com/thomasp85/ggraph/issues/85)).
#'
#' If `codings` are added, the aesthetics are set *for each individual [psOpenSort] separately*, which may make it hard to compare plots across participants.
#' To get consistent code aesthetics, consider applying [ggraph::scale_edge_color_manual()] and friends.
#'
#' @examples
#' ggplot2::autoplot(object = lisa)
#' ggplot2::autoplot(object = rebecca)
#'
#' # no with codes
#' petercodes <- tibble::tibble(category = c("in_homes", "in_homes", "quiet", "herbivore"),
#'                              reference = c("location", "human interaction", NA, "animal diet"),
#'                              length = c("medium", "medium", "medium", "short")
#'                              # notice the duplicates to allow for multiple codes
#'                              )
#' library(ggraph)  # must be attached while running below
#' ggplot2::autoplot(object = peter, edge_codings = petercodes)
#' @export
autoplot.psOpenSort <- function(object, edge_codings = NULL, str_wrap_width = 30) {
  requireNamespace2("ggraph")
  requireNamespace2("igraph")

  dataprep <- tidy.psOpenSort(x = object)
  edge_df <- dataprep$edge_df
  node_df <- dataprep$node_df

  # add codes
  if (!is.null(edge_codings)) {
    # prep codings
    assert_tibble(x = edge_codings, null.ok = TRUE)
    #assert_subset(x = unique(edge_codings[[1]]), choices = unique(edge_df$categories))

    edge_df <- merge(x = edge_df, y = edge_codings, by.x = "categories", by.y = "category", all = TRUE)

    # NAs as nodes don't work, so we have to kill these
    edge_df <- edge_df[!(is.na(edge_df$categories) | is.na(edge_df$items)), ]
    # this implies that missing sub- and supercodes must LATER be added again via the ggplot scale_color function, so that the legend is always complete and color schemes are comparable.
  }

  if (requireNamespace("stringr", quietly = TRUE)) {
    # wrap strings
    node_df$labels <- stringr::str_wrap(string = node_df$labels,
                                        width = str_wrap_width)
  } else {
    warning("Package 'stringr' is not installed, strings could not be wrapped.")
  }

  # replace all NAs with names, because ggplot does not like NAs
  node_df$labels[is.na(node_df$labels)] <- paste(node_df$name[is.na(node_df$labels)], "(NA)")

  graph <- igraph::graph_from_data_frame(d = edge_df, directed = FALSE, vertices = node_df)

  assert_true(x = igraph::bipartite_mapping(graph = graph)$res)

  g <- ggraph::ggraph(graph = graph, layout = "bipartite")
  if (is.null(edge_codings)) {
    g <- g + ggraph::geom_edge_link()
  } else {
    # g <- g + ggraph::geom_edge_fan(mapping = aes_(edge_colour = as.name(colnames(edge_df)[3]), edge_linetype = as.name(colnames(edge_df)[4])))
    g <- g + ggraph::geom_edge_fan(mapping = aes_(edge_colour = as.name(colnames(edge_df)[3])))
  }
  g <- g + ggraph::geom_node_label(mapping = aes(label = labels), repel = FALSE, hjust = "inward")
  g <- g + coord_flip()
  g <- g + theme_void()
  return(g)
}

#' @rdname psOpenSorts
#'
#' @param ... further arguments passed to methods.
#'
#' @examples
#' summary(peter)
#'
#' @export
summary.psOpenSort <- function(object, ...) {
  n_of_cat <- ncol(object)
  n_of_t <- sum(object)

  list(
    n_of_cat = n_of_cat,
    n_of_t = n_of_t,
    avg_t_per_cat = n_of_t / n_of_cat,
    n_of_t_by_item = rowSums(object),
    n_of_t_by_cat = colSums(object)
  )
}

#' @describeIn psOpenSorts *Combine* individual open sorts in a list.
#'
#' @param open_sorts named list of matrices created by [psOpenSort()], one for each participant.
#' Must all be of equal data type and all have the same rows and rownames.
psOpenSorts <- function(open_sorts) {
  validate_psOpenSorts(new_psOpenSorts(open_sorts = open_sorts))
}

# constructor
new_psOpenSorts <- function(open_sorts) {
  structure(
    .Data = open_sorts,
    class = c("psOpenSorts")
  )
}

# validator
validate_psOpenSorts <- function(open_sorts) {
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
    validate_psOpenSort(assignments = x)
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

#' @describeIn psOpenSorts descriptions and *logical* assignments from convenient, but messy format
#'
#' @export
#'
#' @param assignments_messy a character matrix with rows as items, columns as participants and  **logical category assignments** as character strings in cells.
#' Categories are identified by a subset from `LETTERS`, same as in `descriptions_messy`.
#' Assignments must be the same subset of `LETTERS` as the column names in `descriptions_messy`.
#' Rows and columns must be named.
#'
#' For example, if some participant assigned her (self-described) categories `A`, `D` and `Z` to some item, the cell for that item and participant would read `"A, D, Z"`.
#' Order and punctuation are ignored.
#'
#' See `note`.
#'
#' @param descriptions_messy a character matrix with rows as category indices, columns as participants and **category descriptions** in cells.
#' Rows *must* be named by a subset of `LETTERS` to conveniently enter, and identify them from `assignments_messy`.
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
#' @details
#' The canonical representation of open sorts in [psOpenSorts()] can be cumbersome to enter manually.
#' For *logical* (nominally-scaled) open sorts, a simpler, but messier format can be conveniently entered as two separate spreadsheets of `descriptions_messy` and `assignments_messy`.
#'
#' @examples
#'
#' # create psOpenSorts from convenient input
#' ass <- matrix(data = c("A, B",
#'                        # meaning A and B are assigned
#'                        "",
#'                        # meaning no category assigned
#'                        "B",
#'                        # only B assigned
#'                        NA),
#'                        # item never considered for assignment across *all* categories or vice versa
#'                        nrow = 2,
#'                        ncol = 2,
#'                        dimnames = list(items = c("cat", "dog"),
#'                        people = c("tony", "amy")))
#' desc <- matrix(data = c("",
#'                         # will be treated as NA
#'                         NA,
#'                         # participant provided no description, but assigned the category
#'                         "lives in cage",
#'                         # described, but never assigned
#'                         NA,
#'                         # never assigned, never described will be removed
#'                         "actually a predator!",
#'                         "lives on a farm"
#'                         # described, but never assigned
#'                         ),
#'                         nrow = 3,
#'                         dimnames = list(categories = c("A", "B", "C"),
#'                         people = c("tony", "amy")))
#' # notice how individual *nominal* categories are pasted together in cells here;
#' # this convenient form *only* works for nominally-scaled data
#' osorts_example <- import_psOpenSorts(assignments_messy = ass, descriptions_messy = desc)
#'
#' @note
#' When category is assigned, but never described, it is `TRUE` in the respective logical matrix entries and their description is `NA`:
#' This is still considered valuable, if incomplete information.
#' When a category is described, but never assigned, it is omitted from the data entirely.
#'
#' When *no* category was assigned to some item in `assignments_messay`, an empty character string `""` should be in the respective cell.
#'
#' An `NA` value implies that the given participant never considered the given items *at all*, across *all* her categories.
#' Notice this implies *limited scenarios of `NA`* for data entered in this messy, convenient form.
#' The more complicated cases, where a participant did consider *some*, but *not all* items in the assignment of a category, or -- equivalently -- all categories in their assessment of all items, cannot be recorded in this convenience format.
#' Such more granular `NA` records can, however, be recorded in the canonical data representation, where the respective cell of the items x category logical matrix would be `NA`.
#' If your data gathering procedure produces such granular `NA` records, do not use this convenience function.
import_psOpenSorts <- function(assignments_messy, descriptions_messy = NULL) {
  # variable names are too long
  ass <- assignments_messy
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

  # body ====
  # create empty object
  cat_canon <- sapply(X = colnames(ass), FUN = function(x) NULL)

  for (p in names(cat_canon)) {
    max_cats <- LETTERS[LETTERS %in% unlist(strsplit(x = ass[, p], split = ""))]
    # this used to be more complicated
    # we decided that described, but never assigned categories should be omitted.
    # See note in docs.
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
    # let's retain the simple LETTERS, even if they are meaningless, they help with debugging at least
    m <- psOpenSort(assignments = m, descriptions = better_desc[max_cats])  # here kill all the unassigned, but described cats. sad.
    cat_canon[[p]] <- m
  }
  cat_canon <- psOpenSorts(open_sorts = cat_canon)
  return(cat_canon)
}

#' @rdname psOpenSorts
# #' @describeIn psOpenSorts *Summarize* list of open sorts
#'
#' @export
tidy.psOpenSorts <- function(x) {
  by_person <- sapply(X = x, FUN = function(x) unlist(summary(x)[1:3]), simplify = TRUE, USE.NAMES = FALSE)
  by_person <- as.data.frame(t(by_person))
  by_person$name <- rownames(by_person)

  # below two are dicey, because n of cat and n of t is different, so these are unweighted sums
  by_both <- sapply(X = x, FUN = function(x) summary(x)$n_of_t_by_item)
  by_item <- rowSums(x = by_both)

  return(by_person)
}

#' @rdname psOpenSorts
# #' @describeIn psOpenSorts plots Summary
#'
#' @examples
#' ggplot2::autoplot(object = osorts_example)
#'
#' @export
autoplot.psOpenSorts <- function(object) {
  by_person <- tidy.psOpenSorts(x = object)

  g <- ggplot(data = by_person,
              mapping = aes_string(x = 'n_of_cat', y = 'n_of_t', label = 'name'))
  g <- g + geom_point()
  g <- g + xlab("Number of Categories")
  g <- g + ylab("Number of Assignments")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    # repel labels
    g <- g + ggrepel::geom_label_repel()
  } else {
    warning("Package 'ggrepel' is not installed, labels might overplot.")
    g <- g + geom_label()
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
