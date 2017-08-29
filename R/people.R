# PeopleInfo ====

#' @title Check and make PeopleInfo
#'
#' @export
#'
#' @description Checks and makes PeopleInfo, a tibble with arbitrary additional information on the participating people-variables.
#'
#' @param people_info
#' A dataframe or tibble, with one row per participant.
#' First column must be participant name, same as in other objects.
#'
#' @template construct
#'
#' @family import helpers
#'
#' @examples
#' PeopleInfo(data.frame(Name = c("Ann", "Kim", "Joe"),
#'                           Gender = c("female", "other", "male")))
PeopleInfo <- produce_class_constructor(classname = "PeopleInfo", fun = function(people_info) {
  people_info <- tibble::as_tibble(people_info)
  return(people_info)
})

#' @export
#' @rdname check
check.PeopleInfo <- function(x) {
  res <- NULL

  res$tibble <- check_tibble(x = x,
                             types = c("logical", "integer", "integerish", "double", "numeric", "character", "factor"),
                             any.missing = TRUE,
                             all.missing = FALSE,
                             col.names = "strict")
  return(report_checks(res = res, info = "PeopleInfo"))
}
