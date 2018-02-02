# helper ====
#' @title Store all **full items** in a psConcourse matrix.
#'
#' @details
#' Storing **full items** can enable deployment and convenience functions, as well as enhance the presentation of results.
#'
#' **Canonically**, [psConcourse()] stores the concourse as a matrix with:
#' - researcher-facing **item handles** as row names,
#' - **languages** as column names and
#' - participant-facing **full items** in cells.
#'
#' You can also **coerce** other, more convenient forms via [as_psConcourse()]:
#' - `data.frame()`s in the same format,
#' - character vectors of full items, with optional names as handles (for monolingual studies only).
#'
#' @param concourse
#' Participant-facing **full items** as character strings, with researcher-facing **item handles** as names.
#' See `details` for alternative formats.
#' Full items must be unique by language, and can be `NA` if not available (not recommended).
#' Names must be unique and valid R names.
#'
#' @param type a character string giving the *kind* of full item stimuli, must be one of:
#' - `"text"` for textual items, in which case cells in `concourse` must be text.
#'   An additional subclass `"psConcourseText"` is prepended and validated.
#' - `"image"` for image items, in which case cells in `concourse` must be file paths, relative from `img_dir`.
#'   Images must be `*.png`, `*.jpg`, `*.jpeg` or `*.svg`.
#'   An additional class `"psConcourseImage"` is prepended and validated.
#' Defaults to `"text"`.
#'
#' @param markup a character string giving the markup for `type = "text"`s.
#' Defaults to `"plain"` for plain text.
#' Currently only allows `"plain"`.
#' Ignored unless `type = "text"`.
#'
#' @param babel a logical flag, indicating whether [LaTeX's babel package](https://ctan.org/pkg/babel) should be used for multilingual support.
#' If `TRUE` (default), column names in `concourse` must be [valid babel languages](http://ctan.math.washington.edu/tex-archive/macros/latex/required/babel/base/babel.pdf).
#' Ignored unless `type = "text"`.
#'
#' @param img_dir a character string giving the directory for `type = "image"`s.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#' Defaults to `NULL`, in which case images are expected at the working directory root [base::getwd()].
#' Ignored unless `type = "image"`.
#'
#' @return Object of class `psConcourse`.
#'
#' @example tests/testthat/helper_psConcourse.R
#'
#' @template construction_helpers
#'
#' @export
psConcourse <- function(concourse,
                        type = "text",
                        markup = "plain",
                        babel = TRUE,
                        img_dir = NULL) {
  assert_string(x = type, na.ok = FALSE, null.ok = FALSE)

  # construction
  if (type == "text") {
    concourse <- new_psConcourseText(
      concourse = concourse,
      markup = markup,
      babel = babel)
  } else if (type == "image") {
    concourse <- new_psConcourseImage(
      concourse = concourse,
      img_dir = img_dir)
  }

  # validation
  validate_psConcourse(concourse = concourse)

  return(concourse)
}

# parent constructor
new_psConcourse <- function(concourse, ..., subclass = NULL) {
  # base type validation
  assert_matrix(
    x = concourse,
    mode = "character",
    any.missing = TRUE,
    all.missing = TRUE,
    row.names = "strict",
    col.names = "strict",
    null.ok = FALSE)

  structure(
    .Data = concourse,
    ...,
    class = c(subclass, "psConcourse", "matrix"),
    dimnames = list(items = rownames(concourse), languages = colnames(concourse)))
}

# parent validator
validate_psConcourse <- function(concourse) {

  assert_unique_in_column(x = concourse)

  # validate subclasses
  if (inherits(x = concourse, what = "psConcourseText")) {
    validate_psConcourseText(concourse = concourse)
  } else if (inherits(x = concourse, what = "psConcourseImage")) {
    validate_psConcourseImage(concourse = concourse)
  } else {
    stop(
      "No valid type provided. Must be 'text' or 'image'.",
      call. = FALSE)
  }

  return(concourse)
}


# subclass text ====

new_psConcourseText <- function(concourse, markup, babel) {
  new_psConcourse(
    concourse = concourse,
    markup = markup,
    babel = babel,
    subclass = "psConcourseText")
}

validate_psConcourseText <- function(concourse) {
  markup <- attr(x = concourse, which = "markup")
  babel <- attr(x = concourse, which = "babel")
  assert_string(x = markup, na.ok = FALSE, null.ok = FALSE)
  assert_choice(x = markup, choices = c("plain"), null.ok = FALSE)
  assert_flag(x = babel, null.ok = FALSE, na.ok = FALSE)

  if (babel) {
    checkmate::assert_subset(
      x = colnames(concourse),
      choices = latex$options$babel,
      empty.ok = FALSE)
  }
  return(concourse)
}


# subclass image ====

new_psConcourseImage <- function(concourse, img_dir) {
  new_psConcourse(
    concourse = concourse,
    img_dir = img_dir,
    subclass = "psConcourseImage")
}

validate_psConcourseImage <- function(concourse) {
  img_dir <- attr(x = concourse, which = "img_dir")
  assert_string(x = img_dir, na.ok = FALSE, null.ok = TRUE)
  if (!is.null(img_dir)) {
    assert_directory_exists(x = img_dir, access = "r")
    files <- file.path(img_dir, as.vector(concourse))
  } else {
    files <- file.path(as.vector(concourse))
  }
  assert_file_exists(
    x = files,
    extension = c("png", "jpg", "jpeg", "svg"),
    access = "r",
    .var.name = "file names")
  return(concourse)
}


# coercion ====

#' @rdname psConcourse
#'
#' @param languages character vector as an alternative way to give the languages of items.
#' Defaults to `NULL`, in which case languages are expected as (column) names from `concourse`.
#'
#' @param handles character vector as an alternative way to give the item handles.
#' Defaults to `NULL`, in which case items are expected as (row) names from `concourse`.
#'
#' @export
as_psConcourse <- function(concourse,
                           type = "text",
                           markup = "plain",
                           babel = TRUE,
                           img_dir = NULL,
                           languages = NULL,
                           handles = NULL) {
  UseMethod(generic = "as_psConcourse")
}

#' @export
as_psConcourse.default <- function(concourse, type, markup, babel, img_dir, languages, handles) {
  stop_coercion(x = concourse, class = "psConcourse")
}

#' @export
as_psConcourse.psConcourse <- function(concourse, type, markup, babel, img_dir, languages, handles) {
  psConcourse(concourse)
}

#' @describeIn psConcourse coerce matrix to psConcourse
#'
#' @export
as_psConcourse.matrix <- function(concourse,
                                  type = "text",
                                  markup = "plain",
                                  babel = TRUE,
                                  img_dir = NULL,
                                  languages = NULL,
                                  handles = NULL) {
  if (is.data.frame(concourse)) {
    concourse <- as.matrix.data.frame(concourse)
  }

  if (is.character(concourse) & is.vector(concourse)) {
    concourse <- as.matrix(concourse)
  }

  # input validation ===
  assert_matrix(
    x = concourse,
    mode = "character",
    null.ok = FALSE)

  if (!is.null(languages)) {
    assert_character(
      x = languages,
      any.missing = FALSE,
      unique = TRUE,
      len = ncol(concourse))
    assert_names(x = languages, type = "strict")
  }

  if (!is.null(handles)) {
    assert_character(
      x = handles,
      any.missing = FALSE,
      unique = TRUE,
      len = ncol(concourse))
    assert_names(x = handles, type = "strict")
  }

  # make languages ===
  if (!is.null(languages)) {
    if (!is.null(colnames(concourse))) {
      warning("Existing languages as colnames of concourse are overwritten.")
    }
    colnames(concourse) <- languages
  } else if (is.null(colnames(concourse))) {
    stop("No languages as colnames of concourse found.", call. = FALSE)
  }

  # make handles
  if (!is.null(handles)) {
    if (!is.null(rownames(concourse))) {
      warning("Existing handles as rownames of concourse are overwritten.")
    }
    rownames(concourse) <- handles
  } else if (is.null(rownames(concourse))) {
    stop("No languages as rownames of concourse found.", call. = FALSE)
  }

  psConcourse(
    concourse = concourse,
    type = type,
    markup = markup,
    babel = babel,
    img_dir = img_dir)
}


#' @describeIn psConcourse coerce data.frames to psConcourse
#'
#' @export
as_psConcourse.data.frame <- as_psConcourse.matrix


#' @describeIn psConcourse coerce named character vector to psConcourse (monolingual concourse)
#'
#' @export
as_psConcourse.character <- as_psConcourse.matrix


# printing ====

#' @describeIn psConcourse print psConcourse in knitr chunks
#'
#' @template print
#'
#' @export
knit_print.psConcourse <- function(x, use_js = NULL, ...) {
  # Input validation ====
  use_js <- assert_n_infer_use_js(use_js = use_js)

  validate_psConcourse(x)

  # JS method ====
  if (use_js & requireNamespace2(x = "DT", error = FALSE)) {
    DT::datatable(
      data = as.data.frame(x),
      options = list(searchHighlight = TRUE))
  } else {
    requireNamespace2(x = "printr", error = FALSE)
    NextMethod()
  }
}
