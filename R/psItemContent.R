# helper ====
#' @title Store item content as character strings
#'
#' @description
#' Simple helper function to append and validate `psItemContent` class.
#' This class is used as a column class in `psItems`.
#'
#' @param items
#' A character vector giving the *participant-facing* **item content**.
#' Can be named to provide a short, *researcher-facing* **item handles**.
#'
#' @param type a character string giving the *kind* of item content, must be one of:
#' - `"text"` (default) for textual item, in which case `items` must be text.
#'   An additional subclass `"psItemContentText"` is prepended and validated.
#' - `"image"` for imate items, in which case `items` must be file paths, relative from `img_dir`.
#'   Images must be `*.png`, `*.jpg`, `*.jpeg` or `*.svg`.
#'   An additional subclass `"psItemContentImage"` is prepended and validated.
#'
#' @param markup a character string giving the markup for `type = "text"`, must be one of:
#' - `"plain"` (default) fpr plain text.
#' Ignored unless `type = "text"`.
#'
#' @param babel_language a character string giving the language used by [LaTeX's babel package](https://ctan.org/pkg/babel) for multilingual typesetting support.
#' Must be one of:
#' - `NULL` (default), in which case there is no multilingual typesetting support.
#' - a [valid babel languages](http://ctan.math.washington.edu/tex-archive/macros/latex/required/babel/base/babel.pdf).
#' Ignored unless `type = "text"`.
#'
#' @param img_dir a character string giving the directory for `type = "image"`s.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#' Defaults to `NULL`, in which case images are expected at the working directory root [base::getwd()].
#' Ignored unless `type = "image"`.
#'
#' @example tests/testthat/helper_psItemContent.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return A character vector of class `psItems`.
#'
#' @export
psItemContent <- function(items,
                          type = "text",
                          markup = "plain",
                          babel_language = NULL,
                          img_dir = NULL) {
  assert_string(x = type, na.ok = FALSE, null.ok = FALSE)

  # construction
  if (type == "text") {
    items <- new_psItemContentText(
      items = items,
      markup = markup,
      babel_language = babel_language
    )
  } else if (type == "image") {
    items <- new_psItemContentImage(
      items = items,
      img_dir = img_dir
    )
  }

  validate_psItemContent(items = items)

  return(items)
}

# parent constructor
new_psItemContent <- function(items, ..., subclass = NULL) {
  assert_character(
    x = items,
    any.missing = TRUE,
    all.missing = TRUE,
    null.ok = FALSE
  )
  structure(
    .Data = items,
    ...,
    class = c(subclass, "psItemContent", "character")
  )
}

# parent validator
#' @noRd
#' @export
check_S3.psItemContent <- function(x, ps_coll = NULL, ...) {
  assert_character(
    x = x,
    any.missing = TRUE,
    all.missing = TRUE,
    unique = TRUE,
    null.ok = FALSE,
    add = ps_coll,
    .var.name = "items"
  )

  assert_names2(
    x = names(x),
    type = "strict",
    add = ps_coll,
    .var.name = "items"
  )

  NextMethod(ps_coll = ps_coll)
}

validate_psItemContent <- function(items) {
  coll <- makeAssertCollection()

  assert_character(
    x = items,
    any.missing = TRUE,
    all.missing = TRUE,
    unique = TRUE,
    null.ok = FALSE,
    add = coll
  )

  assert_names2(
    x = names(items),
    type = "strict",
    add = coll,
    .var.name = "items"
  )


  # validate subclasses
  # TODO

  reportAssertions(collection = coll)
  return(items)
}

# subclass text ====
new_psItemContentText <- function(items, markup, babel_language) {
  new_psItemContent(
    items = items,
    markup = markup,
    babel_language = babel_language,
    subclass = "psItemContentText"
  )
}

# parent validator
#' @rdname check_S3
#' @export
check_S3.psItemContentText <- function(x, ...) {
  assert_choice(
    x = attr(x = x, which = "markup"),
    choices = c("plain"),
    null.ok = FALSE,
    .var.name = "markup",
    add = ps_coll
  )
  assert_choice(
    x = attr(x = x, which = "babel_language"),
    choices = latex$options$babel,
    null.ok = TRUE,
    add = ps_coll
  )
  NextMethod(ps_coll = ps_coll)
}

validate_psItemContentText <- function(x) {
  coll <- makeAssertCollection()
  assert_choice(
    x = attr(x = x, which = "markup"),
    choices = c("plain"),
    null.ok = FALSE,
    .var.name = "markup",
    add = coll
  )
  assert_choice(
    x = attr(x = x, which = "babel_language"),
    choices = latex$options$babel,
    null.ok = TRUE,
    add = coll
  )
  return(x)
}

new_psItemContentImage <- function(items, img_dir) {
  new_psItemContent(
    items = items,
    img_dir = img_dir,
    subclass = "psItemContentImage"
  )
}

validate_psItemContentImage <- function(x) {
  coll <- makeAssertCollection()
  img_dir <- attr(x = x, which = "img_dir")

  assert_string(x = img_dir, na.ok = FALSE, null.ok = TRUE, add = coll)
  if (!is.null(img_dir)) {
    assert_directory_exists(x = img_dir, access = "r", add = coll)
    files <- file.path(img_dir, as.vector(x))
  } else {
    files <- file.path(as.vector(x))
  }
  assert_file_exists(
    x = files,
    extension = c("png", "jpg", "jpeg", "svg"),
    access = "r",
    .var.name = "file names",
    add = coll)
}
