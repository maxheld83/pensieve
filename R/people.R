# psPeople ====

#' @title Construct tibble with additional participant information
#'
#' @export
#'
#' @param people
#' A dataframe or tibble, with one row per participant.
#' First column must be (short form of) participant name and valid R name.
#' Later columns can have arbitrary additional information about participants.
#' Columns must be named.
#'
#' @template construction_helpers
#'
#' @examples
#' people <- psPeople(data.frame(Name = c("Ann", "Kim", "Joe"),
#'                                     Gender = c("female", "other", "male"),
#'                                     stringsAsFactors = FALSE))
psPeople <- function(people) {
  people <- new_psPeople(people = people)
  people <- validate_psPeople(people = people)
  return(people)
}

# constructor
new_psPeople <- function(people) {
  people <- tibble::as_tibble(people)
  structure(
    .Data = people,
    class = c("psPeople", class(people))
  )
}

# validator
validate_psPeople <- function(people) {
  check_tibble(x = people,
               types = c("logical", "integer", "integerish", "double", "numeric", "character", "factor"),
               any.missing = TRUE,
               all.missing = FALSE,
               col.names = "strict")
  names <- people[[1]]
  check_names(x = names, type = "strict")
  return(people)
}

