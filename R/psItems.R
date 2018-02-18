# items ====

#' @title Construct list of item-related objects
#'
#' @export
#'
#' @param ps_concourse object returned by [psConcourse()]
#'
#' @template construction_helpers
psItems <- function(ps_concourse) {
  validate_psItems(new_psItems(ps_concourse = ps_concourse))
}

# constructor
new_psItems <- function(ps_concourse) {
  structure(
    .Data = list(
      concourse = ps_concourse
    ),
    class = c("psItems")
  )
}

# validator
validate_psItems <- function(ps_items) {
  validate_psConcourse(ps_items$concourse)  # this also validates subclass, must not be null
  return(ps_items)
}

# Drafts and Helpers ====
# helper: check QLookup
check_QLookup <- check_lookup <- function(x){
  res <- NULL
  res$tibble <- check_tibble(x = x,
                             types = c("integerish", "integer", "character"),
                             any.missing = TRUE,  # some NAs are permissible
                             all.missing = FALSE,
                             null.ok = FALSE)

  res$nna_row <- check_nna_row(x = x[,-1])  # we do not care about first column

  # check whether the lookup table is all unique by columns, as is must be
  # notice the being non-unique by row does not matter
  res$unique_column <- check_unique_in_column(x = x)

  return(report_checks(res = res, info = "lookup"))
}


# helper: check Q_set vs lookup
check_qset_v_lookup <- function(q_set, lookup) {
  if (all(q_set %in% lookup$item_handle)) {
    return(TRUE)
  } else {
    return(paste("All item handles in",
                 vname(q_set),
                 "must be in",
                 vname(lookup)))
  }
}


# TODO this would be a helpful hint for people who only have bad q_set
# #' @importFrom lettercase make_names
# #' @name make_names
# #' @rdname make_names
# #' @export
# NULL
