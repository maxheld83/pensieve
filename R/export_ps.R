#' @title
#' Export S3 objects from pensieve in useful formats.
#'
#' @description
#' Writes S3 objects from pensieve to the file system in some selected formats.
#'
#' @param x An object with one of the pensieve S3 classes.
#'
#' @param dir `[character(1)]`
#' giving the directory where the exported objects are written to.
#' Must be relative from the working directory.
#' Defaults to the working directory root.
#'
#' @param ... further arguments to be passed to methods.
#'
#' @return `[character()]`
#' (invisible) giving the paths to the exported files.
#'
#' @family export functions
#'
#' @export
export_ps <- function(x, dir = ".", ...) {
  assert_directory(x = dir, access = "w")
  UseMethod(generic = "export_ps")
}

#' @rdname export_ps
#' @export
export_ps.default <- function(x, dir, ...) {
  stop(
    "Can't find an export method for any of these classes: ",
    glue::glue_collapse(class(x), sep = ", ", last = " and "),
    ". ",
    call. = FALSE
  )
}
