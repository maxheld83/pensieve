# helper ====
#' @title Store additional arbitrary information about psConcourse items
#'
#' @param concourse_extra
#' A data frame or tibble with additional information.
#' There must be as many rows as rows in [psConcourse()], and in the same order.
#' If there is a first column named `items`, it must be the same as the rownames of [psConcourse()].
#'
#' @details
#'
#' @return A tibble of class `psConcourseExtra`
#'
#' @export
psConcourseExtra <- function(concourse_extra) {
  validate_psConcourseExtra(new_psConcourseExtra(concourse_extra))
}

new_psConcourseExtra <- function(concourse_extra) {
  structure(
    .Data = concourse_extra,
    class = c("psConcourseExtra", "tbl_df", "tbl", "data.frame")
  )
}

validate_psConcourseExtra <- function(concourse_extra) {
  assert_tibble(
    x = concourse_extra,
    any.missing = TRUE,
    all.missing = TRUE,
    null.ok = FALSE)

  return(concourse_extra)
}

