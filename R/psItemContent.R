# helper ====
#' @title
#' Store item content as character strings
#'
#' @description
#' Helper function to append and validate [`psItemContent`][psItemContent] class.
# TODO link to psItems class here, once available
#' This class is used as a column class in `psItems`.`
#'
#' @details
#' Storing **full items** can enable deployment and convenience functions, as well as enhance the presentation of results.
#'
#' @param items
#' `[character()]` giving the *participant-facing* **item content**.
#' Can be named to provide short, *researcher-facing* **item handles**.
#'
#' @param type
#' `[character(1)]` giving the *kind* of item content, must be one of:
#' - `"text"` (default) for textual item, in which case `items` must be text.
#'   Items can be marked up using [Pandoc Markdown](https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html).
#'   An additional subclass `psItemContentText` is prepended and validated.
#' - `"image"` for image items, in which case `items` must be file paths, relative from `img_dir`.
#'   Images must be `*.png`, `*.jpg`, `*.jpeg` or `*.svg`.
#'   An additional subclass `psItemContentImage`` is prepended and validated.
#'
#' @param lang
#' `[character(1)]` giving a language code for *all* items, such as `en_US`.
#' Used for multilingual typsetting support via [LaTeX's babel package](https://ctan.org/pkg/babel).
#' Must be one of:
#' - `NULL` (default), in which case there is no multilingual typesetting support.
#' - a [valid BCP 47 language code](https://tools.ietf.org/html/bcp47) supported by pandoc.
#'   See `pensieve:::langs` for all available languages.
#' Ignored unless `type = "text"`.
#'
#' @param img_dir
#' `[character(1)]` giving the directory for `type = "image"`s.
#' Defaults to `NULL`, in which case images are expected at the working directory root [base::getwd()].
#' Ignored unless `type = "image"`.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#'
#' @example tests/testthat/helper_psItemContent.R
#'
#' @family S3 classes from `pensieve`
#'
#' @return
#' `[character()]` with class [`psItemContent`][psItemContent].
#'
#' @export
psItemContent <- function(items,
                          type = "text",
                          lang = NULL,
                          img_dir = NULL) {
  assert_string(x = type, na.ok = FALSE, null.ok = FALSE)

  # construction
  if (type == "text") {
    items <- new_psItemContentText(
      items = items,
      lang = lang
    )
  } else if (type == "image") {
    items <- new_psItemContentImage(
      items = items,
      img_dir = img_dir
    )
  }

  assert_S3(x = items)

  return(items)
}

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

#' @describeIn psItemContent Validation
#' @inheritParams validate_S3
#' @export
validate_S3.psItemContent <- function(x, ps_coll = NULL, ...) {
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


# subclass text ====
new_psItemContentText <- function(items, lang) {
  new_psItemContent(
    items = items,
    lang = lang,
    subclass = "psItemContentText"
  )
}

#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentText <- function(x, ...) {
  assert_choice(
    x = attr(x = x, which = "lang"),
    choices = langs,
    null.ok = TRUE,
    .var.name = "lang")

  NextMethod(ps_coll = ps_coll)
}

# subclass images ====
new_psItemContentImage <- function(items, img_dir) {
  new_psItemContent(
    items = items,
    img_dir = img_dir,
    subclass = "psItemContentImage"
  )
}

#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentImage <- function(x, ...) {
  img_dir <- attr(x = x, which = "img_dir")

  assert_string(
    x = img_dir,
    na.ok = FALSE,
    null.ok = TRUE,
    add = ps_coll)

  if (!is.null(img_dir)) {
    assert_directory_exists(x = img_dir, access = "r", add = ps_coll)
    files <- file.path(img_dir, as.vector(x))
  } else {
    files <- file.path(as.vector(x))
  }
  assert_file_exists(
    x = files,
    extension = c("png", "jpg", "jpeg", "svg"),
    access = "r",
    .var.name = "file names",
    add = ps_coll)

  NextMethod(ps_coll = ps_coll)
}

# rendering ====
#' @title Render text items to pdf and svg.
#'
#' @description
#' Renders character vectors of items to a pdf, using pandoc and latex, then converts pdf to svg using pdf2svg.
#'
#' @details
#' It is often helpful to have an authoritative, and professionally typeset version of items.
#' This function renders items.
#' When `items` are named with item handles, such handles are used as file names.
#' Because items always have to fit on one page, this function errors out when the rendered item would fill more than one page.
#'
#' @inheritParams psItemContent
#'
#' @param output_dir `[character(1)]`
#' giving directory relative from working directory root [base::getwd()].
#' Best constructed with [base::file.path()].
#' Defaults to `NULL`, in which case items are rendered to the working directory root.
#'
#' @param fontsize `[character(1)]`
#' giving a [LaTeX fontsize](https://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text) for *all* items.
#' See `pensieve:::latex$options$fontsize` for valid options.
#' Defaults to `NULL`, in which case the maximum possible fontsize is sought and chosen, by which *all* items still fit on one page.
#'
#' @param paperwidth `[numeric(1)]` giving the width of cards in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' For good typographical results, should be as close as possible to the *actual* physical measurements of cards encountered by users.
#' Defaults to `8.5`.
#' @param paperheight `[numeric(1)]` giving the height of cards in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' For good typographical results, should be as close as possible to the *actual* physical measurements of cards encountered by users.
#' Defaults to `5.4`.
#'
#' @param top `[numeric(1)]` giving the margin in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' Defaults to `0.5`.
#' @param bottom `[numeric(1)]` giving the margin in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' Defaults to `0.5`.
#' @param left `[numeric(1)]` giving the margin in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' Defaults to `0.5`.
#' @param right `[numeric(1)]` giving the margin in `units`, passed on to [LaTeX geometry package](https://ctan.org/pkg/geometry).
#' Defaults to `0.5`.
#'
#' @param units `[character(1)]` giving the units for the above dimensions, must be one of:
#' - "cm" for metric system,
#' - "in" for inches.
#' Defaults to `"cm"`.
#'
#' @param alignment `[character(1)]` giving the alignment for the text, must be one of:
#' - "justified",
#' - "left",
#' - "right" or
#' - "center".
#' Defaults to `"left"`.
#'
#' @export
render_items <- function(items,
                         lang = NULL,
                         output_dir = NULL,
                         fontsize = NULL,
                         paperwidth = 8.5,
                         paperheight = 5.4,
                         top = 0.5,
                         bottom = 0.5,
                         left = 0.5,
                         right = 0.5,
                         units = "cm",
                         alignment = "left") {
}

# test <- md2tex(
#   text = "foo",
#   lang = "en-US",
#   fontsize = "normalsize",
#   paperwidth = 8.5,
#   paperheight = 5.4,
#   top = 0.5,
#   bottom = 0.5,
#   left = 0.5,
#   right = 0.5,
#   units = "cm",
#   alignment = "left")


md2tex <- function(text,
                   lang,
                   fontsize,
                   paperwidth,
                   paperheight,
                   top,
                   bottom,
                   left,
                   right,
                   units,
                   alignment) {

  # input validation
  assert_string(x = text, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  # other arguments are treated downstream

  # check dependencies
  assert_sysdep(x = "pandoc")

  # render string to latex
  system2(
    command = "pandoc",
    input = c(
      latex$set$fontsize(fontsize = fontsize),
      latex$set$alignment(alignment = alignment),
      text
    ),
    args = c(
      "--from=markdown",  # this is pandoc's extended markdown
      "--to=latex",  # redirects to stdout
      "--verbose",  # for debugging
      "--standalone",  # uses template

      # geometry options
      latex$set$geometry(
        paperwidth = paperwidth,
        paperheight = paperheight,
        top = top,
        bottom = bottom,
        left = left,
        right = right,
        units = units,
        vcentering = TRUE,
        hcentering = TRUE),

      # other latex options
      "-V pagestyle=empty",

      # language
      glue::glue("-V lang={lang}")
    ),
    stdout = TRUE,
    stderr = "",
    wait = TRUE
  )
}

# helpers to create latex formatting instructions
latex <- list(set = NULL, # these are the functions
              options = NULL) # these are the available options

#' @title Generate pandoc geometry options
#' @inheritParams render_item
#' @param vcentering `[logical(1)]` whether to center vertically, does not currently work.
#' @param vcentering `[logical(1)]` whether to center horizontally, does not currently work.
#' @noRd
latex$set$geometry <- function(paperwidth, paperheight, top, bottom, left, right, units, vcentering, hcentering) {
  # input validation
  assert_string(x = units, na.ok = FALSE, null.ok = FALSE)
  assert_choice(x = units, choices = c("cm", "in"))
  assert_flag(x = vcentering, na.ok = FALSE, null.ok = FALSE)
  assert_flag(x = hcentering, na.ok = FALSE, null.ok = FALSE)

  num_arguments <- list(
    paperwidth = paperwidth,
    paperheight = paperheight,
    top = top,
    bottom = bottom,
    left = left,
    right = right
  )
  res <- purrr::map2_chr(
    .x = num_arguments,
    .y = names(num_arguments),
    .f = function(x, y) {
      # this is input validation
      assert_numeric(x = x, lower = 0, finite = TRUE, any.missing = FALSE, len = 1, null.ok = FALSE, .var.name = y)

      # this is the actual paste job
      paste0(
        "-V geometry:",
        y,
        "=",
        x,
        units
      )
    }
  )
  if (vcentering) res <- c(res, vcentering = "-V geometry:vcentering")
  if (hcentering) res <- c(res, hcentering = "-V geometry:hcentering")
  return(res)
}

latex$options$fontsize <- c(
  # this list is from https://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text
  # must remain in ascending order!
  "tiny",
  "scriptsize",
  "footnotesize",
  "small",
  "normalsize",
  "large",
  "Large",
  "LARGE",
  "huge",
  "Huge"
)
#' @title Generate latex fontsize command
#' @inheritParams render_item
#' @noRd
latex$set$fontsize <- function(fontsize) {
  checkmate::assert_character(x = fontsize, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = fontsize, choices = latex$options$fontsize, null.ok = FALSE)

  paste0("\\", fontsize)
}

# insert arbitrary alignment
latex$options$alignment <- c(
  # this list is from https://www.sharelatex.com/learn/Text_alignment
  # we're only using vanilla latex, no extra package
  "justified",
  "left",
  "right",
  "center"
)
#' @title Generate latex alignment command
#' @inheritParams render_item
#' @noRd
latex$set$alignment <- function(alignment = "justified") {
  # alignment <- "justified"
  checkmate::assert_character(x = alignment, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = alignment, choices = latex$options$alignment, null.ok = FALSE)

  switch(
    EXPR = alignment,
    left = return("\\raggedright"),
    right = return("\\raggedleft"),
    center = return("\\centering")
  )
}

# generally, the BCP 47 standard allows all manner of language, region combinations (and more), e.g. "de_AT"
# however, only a subset is allowed in pandoc and translated to panglossia or babel
# this is (unfortunately) transcribed from the haskell script inside pandoc
# https://github.com/jgm/pandoc/blob/b8ffd834cff717fe424f22e506351f2ecec4655a/src/Text/Pandoc/Writers/LaTeX.hs#L1354-L1480
langs <- readr::read_delim(
  file = system.file("extdata", "langs.csv", package = "pensieve"),
  col_names = TRUE,
  delim = ",",
  col_types = "ccccll"
)
# this can't be right
langs <- purrr::pmap(.l = langs[,c("lang_short", "var_short", "lang_long", "var_long")], .f = function(lang_short, var_short, lang_long, var_long) {
  if (is.na(var_short)) {
    short <- lang_short
  } else {
    short <- glue::glue('{lang_short}-{var_short}')
  }
  if (is.na(var_long)) {
    long <- lang_long
  } else {
    long <- glue::glue('{lang_long} ({var_long})')
  }
  names(short) <- long
  return(short)
})
langs <- purrr::as_vector(langs)


# in contrast to normal texi function, this returns the pdf as a raw vector
texi2pdf_raw <- function(tex) {
  requireNamespace2(x = "tools")
  requireNamespace2(x = "fs")
  requireNamespace2(x = "withr")

  # make sure file gets deleted again
  # this is not a tempfile, but just from wd, which might be helpful for debugging
  withr::local_file(
    file = c("item.tex", "item.pdf")
  )

  write(x = tex, file = "item.tex")
  # availability of tex will ideally be checked in platform dependent way by texi2pdf
  tools::texi2pdf(file = "item.tex", clean = TRUE, index = FALSE, quiet = TRUE)

  readBin(
    con = "item.pdf",
    what = "raw",
    n = fs::file_info("item.pdf")$size  # need to allocate size
  )
}

#pdf_raw <- texi2pdf_raw(tex = test)
#svg_raw <- pdf2svg_raw(pdf_raw = pdf_raw)

check_pdf1page <- function(x) {
  requireNamespace2(x = "pdftools")
  infos <- pdftools::pdf_info(pdf = x)
  if (infos$pages == 1) {
    return(TRUE)
  } else {
    return("PDF must be 1 page long.")
  }
}
assert_pdf1page <- checkmate::makeAssertionFunction(check.fun = check_pdf1page)
test_pdf1page <- checkmate::makeTestFunction(check.fun = check_pdf1page)
expect_pdf1page <- checkmate::makeExpectationFunction(check.fun = check_pdf1page)

# helper to convert pdf to svg
pdf2svg <- function(pdf_input) {
  requireNamespace2(x = "fs")
  requireNamespace2(x = "withr")
  requireNamespace2(x = "tools")
  # dependencies
  checkmate::assert_os(os = c("mac", "linux"))
  assert_sysdep(x = "pdf2svg")

  # input validation
  checkmate::assert_file_exists(x = pdf_input, extension = "pdf")
  checkmate::assert_character(x = pdf_input, any.missing = FALSE, unique = TRUE)

  svg_output <- paste0(
    tools::file_path_sans_ext(pdf_input),
    ".svg"
  )
  system2(command = "pdf2svg",
          args = c(pdf_input,
                   svg_output,
                   "1"),
          stderr = "")  # only take 1st page
}
pdf2svg_raw <- function(pdf_raw) {
  withr::local_file(
    file = c("item.pdf", "item.svg")
  )
  writeBin(object = pdf_raw, con = "item.pdf")
  pdf2svg(pdf_input = "item.pdf")
  readBin(
    con = "item.svg",
    what = "raw",
    n = fs::file_info("item.svg")$size  # need to allocate size
  )
}
