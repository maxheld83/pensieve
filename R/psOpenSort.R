# helper ====
#' @title Store *individual* sort and dimension description by *one* participant.
#'
#' @aliases psLogicalOpenSort psOrdinalOpenSort psIntervalOpenSort
#'
#' @param osort
#' a matrix with items as rows, open dimensions as columns, and open sort value in cells.
#'
#' If rows are named (by item handles), names must be valid R names.
#'
#' If columns are named, they must be valid R names and they must be the same as the names in `descriptions`.
#' Either way, `osort` and `descriptions` are always *matched by index only*: the first column from `osort`, must be the first element of `description`, and so forth.
#'
#' @param descriptions
#' a character vector giving the open-ended dimension description provided by the participant.
#' If elements are named, names must be valid R names.
#' Defaults to `NULL`, in which case no user-provided dimension descriptions are available (not recommended).
#'
#' @param scale a charater string giving the *scale* of the open sorts, must be one of:
#' - `logical` for *nominally*-scaled sort, where an open dimension applies (`TRUE`) or does not apply (`FALSE`).
#'   `osort` must be a logical matrix.
#'   The subclass `"psLogicalOpenSort"` is prepended and validated.
#' - `ordinal` for an *ordinally*-scaled sort, where an open dimension applies to some item *more* (`2nd` rank) *or less* (`3rd` rank) than to another other item.
#'    `osort` must be an `integer` matrix.
#'    The subclass `"psOrdinalOpenSort"` is prepended and validated.
#' - `interval` for *interval* or *ratio*-scaled sort, where an open dimension applies to some item *by some amount more or less* (say `2.4` units) than to another item.
#'    `osort` must be a `numeric` matrix.
#'    The subclass `"psIntervalOpenSort"` is prepended and validated.
#'
#' Defaults to `NULL`, in which case the scale is inferred from the implicit class of `osort`.
#'
#' Currently only `logical` is supported.
#'
#' @return Object of class `psOpenSort`.
#'
#' @example tests/testthat/helper_psOpenSort.R
#'
#' @template construction_helpers
#'
#' @export
psOpenSort <- function(osort, descriptions = NULL, scale = NULL) {
  descriptions <- as.list(descriptions)

  assert_string(x = scale, na.ok = FALSE, null.ok = TRUE)

  # find implicit class
  if (is.null(scale)) {
    # switch on typeof or mode doesn't work; yields inconsistent results
    if (is.logical(osort)) {
      scale <- "logical"
    } else if (is.integer(osort)) {
      scale <- "ordinal"
    } else if (is.numeric(osort)) {
      scale <- "interval"
    }
  }

  subclass <- os_subcl[os_subcl$scale == scale, "class_singular"]

  validate_psOpenSort(new_psOpenSort(
    osort = osort,
    descriptions = descriptions,
    subclass = subclass))
}

# parent constructor
new_psOpenSort <- function(osort, descriptions, subclass) {
  do.call(what = structure, args = append(
    x = list(.Data = osort,
             dimnames = list(items = rownames(osort), dimensions = colnames(osort)),
             class = c(subclass, "psOpenSort", "matrix")),
    values = list(descriptions = descriptions)))
}

# parent validator
validate_psOpenSort <- function(osort) {
  # validate subclasses
  which_cl <- as.logical(inherits(x = osort, what = os_subcl$class_singular, which = TRUE))
  if (!any(which_cl)) {
    stop("No valid subclass found.")
  }

  # check base type
  mode <- os_subcl[which_cl, "mode"]
  assert_matrix(x = osort,
                any.missing = TRUE,
                all.missing = TRUE,
                null.ok = FALSE,
                mode = mode)

  # validate osort
  assert_set_equal(x = names(dimnames(osort)), y = c("items", "dimensions"))
  assert_names2(x = colnames(osort))
  assert_names2(x = rownames(osort))

  # there is no meaningful information in this case
  for (c in 1:ncol(osort)) {
    assert_var(
      x = osort[,c],
      .var.name = colnames(x = osort, do.NULL = FALSE, prefix = "Column ")[c]
    )
  }

  # recreate descriptions
  descriptions <- attributes(osort)$descriptions
  if (length(descriptions) == 0) {  # recreate NULL assignment, when there are none in attr
    descriptions <- NULL
  }

  # validate descriptions
  if (!is.null(descriptions)) {
    assert_list(x = descriptions,
                types = "character",
                any.missing = TRUE,
                all.missing = TRUE,
                unique = FALSE,  # oddly, duplicate NAs count as non-unique, hence extra test below
                null.ok = FALSE,
                len = ncol(osort)) # this already validates against osort
    assert_names2(names(descriptions))

    # must test if non-NAs are at least unique
    assert_list(x = descriptions[!(is.na(descriptions))],
                unique = TRUE)

    if (!is.null(colnames(osort)) & !is.null(names(descriptions))) {
      # validate descriptions AND osort names (but matching is always by index only)
      assert_set_equal(x = names(descriptions),
                       y = colnames(osort),
                       ordered = TRUE)
    }
  }
  return(osort)
}

# subclass logical ====

# see above

# subclass ordinal ====

# see above

# subclass interval ====

# see above

# setting up all the names to avoid repetitions
os_subcl <- data.frame(
  scale = c("logical", "ordinal", "interval"),
  mode = c("logical", "integer", "numeric"),
  class_singular = c("psLogicalOpenSort", "psOrdinalOpenSort", "psIntervalOpenSort"),
  stringsAsFactors = FALSE
)
os_subcl$class_plural <- paste0(os_subcl$class_singular, "s")
os_subcl


# coercion ====

#' @rdname psOpenSort
#'
#' @export
as_psOpenSort <- function(osort, descriptions = NULL, scale = NULL) {
  UseMethod(generic = "as_psOpenSort", object = osort)
}

#' @export
as_psOpenSort.default <- function(osort, descriptions = NULL, scale = NULL) {
  stop_coercion(x = osort, class = "psOpenSort")
}

#' @export
as_psOpenSort.psOpenSort <- function(osort, descriptions = NULL, scale = NULL) {

  # these are already in osort, as is plausibe for psOpenSort objects (!)
  desc_in_ass <- unlist(attributes(osort)$descriptions[])
  if (is.null(descriptions)) {
    descriptions <- desc_in_ass
  } else {
    if (identical(x = descriptions, y = desc_in_ass)) {
      # no problem here, b/c they are the same
    } else {
      warning(paste(
        "Existing descriptions in 'osort' overwritten with 'descriptions' argument."
      ))
    }
  }

  psOpenSort(osort = osort, descriptions = descriptions, scale = scale)
}

#' @describeIn psOpenSort coerce matrix to psOpenSort
#'
#' @export
as_psOpenSort.matrix <- function(osort, descriptions = NULL, scale = NULL) {
  # take care of data frame inputs
  m <- as.matrix(osort)

  # now set zero-var columns to NA (just for convenience)
  desc <- descriptions
  kill_cs <- NULL
  for (c in 1:ncol(m)) {
    # TODO a proper cbind method might do this better.
    if (!test_var(m[,c])) {
      if (!is.null(desc) & !is.na(desc[c])) {
        # if there is at least a description, we keep the column and NA it
        m[,c] <- NA
        warning(paste(
          "Column",
          colnames(x = m, do.NULL = FALSE)[c],
          "has no variance, but there is a corresponding description.",
          "The description has been retained, and the column set to all 'NA's."
        ))
      } else {
        warning(paste(
          "Column",
          colnames(x = m, do.NULL = FALSE)[c],
          "has been dropped, because it has no variance nor description."
        ))
        kill_cs <- c(kill_cs, c)  # kill these columns
      }
    }
  }
  if (!is.null(kill_cs)) {
    m <- as.matrix(m[, -kill_cs])
    if (!is.null(desc)) {  # if applicable, also remove corresponding desc
      desc <- desc[-kill_cs]
    }
  }

  psOpenSort(osort = m, descriptions = desc, scale = scale)
}

#' @describeIn psOpenSort coerce data.frame to psOpenSort
#'
#' @export
as_psOpenSort.data.frame <- as_psOpenSort.matrix


# plotting ====
#' @describeIn psOpenSort Prepare *individual* open sort for bipartite plotting.
#'
#' @param x a [psLogicalOpenSort], created by [psOpenSort()].
#'
#' @export
tidy.psLogicalOpenSort <- function(x) {
  # input validation ====
  assert_class(x = x, classes = c("psLogicalOpenSort", "psOpenSort"), null.ok = FALSE)
  #TODO replace this with a real asserter, once available
  invisible(validate_psOpenSort(osort = x))

  # give x safe row and colnames so that downstream functions have meaningful names
  rownames(x) <- rownames(x = x, do.NULL = FALSE, prefix = "it")
  colnames(x) <- colnames(x = x, do.NULL = FALSE, prefix = "dim")

  # prep EDGE data ====
  # melt the edges
  edge_df <- reshape2::melt(data = x, as.is = TRUE)
  edge_df <- edge_df[!(is.na(edge_df$value)), ]  # kill NAs
  edge_df <- edge_df[edge_df$value, ]  # take only TRUEs
  edge_df <- edge_df[, c("items", "dimensions")]

  # NAs as nodes don't work, so we have to kill these
  edge_df <- edge_df[!(is.na(edge_df$dimensions) | is.na(edge_df$items)), ]
  # this implies that missing sub- and supercodes must LATER be added again via the ggplot scale_color function, so that the legend is always complete and color schemes are comparable.

  # prep safe rownames of x

  # prep NODE VERTICES DATA ====
  fulldesc <- unlist(attributes(x)$descriptions[])

  # but for dims, sometimes there *are* descriptions
  if (is.null(fulldesc)) {
    labels <- c(rownames(x), colnames(x))
  } else {
    labels <- c(rownames(x), fulldesc)
  }
  node_df <- data.frame(
    name = c(rownames(x), colnames(x = x)),
    type = c(rep(TRUE, nrow(x)), rep(FALSE, ncol(x))),
    labels = labels,
    stringsAsFactors = FALSE
  )

  return(list(edge_df = edge_df, node_df = node_df))
}


#' @describeIn psOpenSort Create bipartite plot from *individual* open sort.
#'
#' @param object a [psLogicalOpenSort], created by [psOpenSort()].
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
#' # plotting ====
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
autoplot.psLogicalOpenSort <- function(object, edge_codings = NULL, str_wrap_width = 30) {
  requireNamespace2("ggraph")
  requireNamespace2("igraph")

  dataprep <- tidy.psLogicalOpenSort(x = object)
  edge_df <- dataprep$edge_df
  node_df <- dataprep$node_df

  # add codes
  if (!is.null(edge_codings)) {
    # prep codings
    assert_tibble(x = edge_codings, null.ok = TRUE)
    #assert_subset(x = unique(edge_codings[[1]]), choices = unique(edge_df$categories))

    edge_df <- merge(x = edge_df, y = edge_codings, by.x = "dimensions", by.y = "category", all = TRUE)

    # NAs as nodes don't work, so we have to kill these
    edge_df <- edge_df[!(is.na(edge_df$dimensions) | is.na(edge_df$items)), ]
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


# summary ====

#' @describeIn psOpenSort Summarise an *individual* open sort.
#'
#' @param ... further arguments passed to methods.
#'
#' @examples
#' summary(peter)
#'
#' @export
summary.psLogicalOpenSort <- function(object, ...) {
  n_dim <- ncol(object)
  n_true <- sum(object, na.rm = TRUE)

  list(
    n_dim = n_dim,
    n_true = n_true,
    true_per_dim = n_true / n_dim,
    n_true_by_item = rowSums(object, na.rm = TRUE),
    n_true_by_dim = colSums(object, na.rm = TRUE)
  )
}
