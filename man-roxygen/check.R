#' @description Check S3 class.
#'
#' @param x class object created by respective constructor function.
#'
#' @family validation functions
#'
#' @examples
#' # create checkable object
#' x <- QItemConcourse(
#'   concourse = matrix(
#'     data = c(
#'       "Man lives to work.", "Man lebt, um zu arbeiten.",
#'       "Man works to live.", "Man arbeitet, um zu leben."
#'     ),
#'     nrow = 2, ncol = 2,
#'     dimnames = list(
#'       items = c("live_2_work", "work_2_live"),
#'       languages = c("english", "ngerman")  # ideally, these are valid babel languages
#'     )
#'   )
#' )
#'
#' # check object and friends ...
#' check(x)  # returns TRUE or error message
#' test(x)  # returns TRUE or FALSE
#' assert(x)  # returns error or silently object
