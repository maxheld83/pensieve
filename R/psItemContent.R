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
#' - if `dir_bin` is `NULL` (default), `items` must be text.
#'   Items can be marked up using [Pandoc Markdown](https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html).
#'   An additional subclass `psItemContentText` is prepended and validated.
#' - if `dir_bin` is given, `items` must be file paths, relative from `dir_bin`.
#'   An additional subclass `psItemContentBin`` is prepended and validated.
#'   `lang` and `alignment` are ignored.
#'
#' @param dir_bin
#' `[character(1)]` giving the root from which `items` can be found, when `items` are paths.
#' Defaults to `NULL`, in which case `items` are expected to be texts.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#'
#' @inheritParams declare_pandoc_var
#' @inheritParams md2tex
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
                          dir_bin = NULL,
                          lang = NULL,
                          paperwidth = 8.5,
                          paperheight = 5.4,
                          top = 0.5,
                          bottom = 0.5,
                          left = 0.5,
                          right = 0.5,
                          unit = "cm",
                          vcentering = TRUE,
                          hcentering = TRUE,
                          alignment = "justified") {
  assert_string(x = dir_bin, na.ok = FALSE, null.ok = TRUE)

  geometry_opts <- list(
    paperwidth = paperwidth,
    paperheight = paperheight,
    top = top,
    bottom = bottom,
    left = left,
    right = right,
    vcentering = vcentering,
    hcentering = hcentering
  )

  # construction
  if (is.null(dir_bin)) {
    items <- new_psItemContentText(
      items = items,
      lang = lang,
      geometry_opts = geometry_opts,
      alignment = alignment
    )
  } else {
    items <- new_psItemContentBin(
      items = items,
      dir_bin = dir_bin,
      geometry_opts = geometry_opts
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

  geometry_opts <- attr(x = x, which = "geometry_opts")
  if (test_sysdep(x = "pandoc")) {
    assert_fun_args(x = do.call, y = md2tex_mem, args = c(x = "foo", geometry_opts), add = ps_coll, .var.name = "md2tex_mem")
  }

  NextMethod(ps_coll = ps_coll)
}

# coercion ====
#' @rdname psItemContent
#' @param obj
#' An object which can be coerced to [psItemContent][psItemContent], currently one of:
#' - a (named) character vector.
#' @export
as_psItemContent <- function(obj, ...) {
  UseMethod("as_psItemContent")
}
as_psItemContent.default <- function(obj, ...) {
  stop_coercion(obj = obj, target_class = "psItemContent")
}
as_psItemContent.psItemContent <- function(obj, ...) {
  assert_S3(x = obj)
  obj
}
as_psItemContent.character <- function(obj, ...) {
  psItemContent(items = obj, ...)
}


# subsetting ====
#' @title Subsetting method for psItemContents
#' @description Replacement method for subsetting to retain attributes
#' @inheritParams psItemContent
#' @inheritParams  base::Extract
#' @export
#' @noRd
`[.psItemContentText` <- function(x, i, ...) {
  new_psItemContent(
    items = NextMethod(x),
    lang = attr(x, "lang"),
    geometry_opts = attr(x, "geometry_opts"),
    alignment = attr(x, "alignment"),
    subclass = "psItemContentText"
  )
}
`[.psItemContentBin` <- function(x, i, ...) {
  new_psItemContent(
    items = NextMethod(x),
    dir_bin = attr(x, "dir_bin"),
    geometry_opts = attr(x, "geometry_opts"),
    subclass = "psItemContentBin"
  )
}

# subclass text ====
new_psItemContentText <- function(items, lang, geometry_opts, alignment) {
  new_psItemContent(
    items = items,
    lang = lang,
    geometry_opts = geometry_opts,
    alignment = alignment,
    subclass = "psItemContentText"
  )
}

#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentText <- function(x, ...) {
  lang <- attr(x = x, which = "lang")
  alignment <- attr(x = x, which = "alignment")

  if (test_sysdep(x = "pandoc")) {
    assert_fun_args(x = md2tex_mem, y = x[1], lang = lang, add = ps_coll)
  }

  NextMethod(ps_coll = ps_coll)
}

# knit_print ====
#' @describeIn psItemContent Printing inside knitr chunks
#' @template knit_print
#' @export
knit_print.psItemContentText <- function(x,
                                         inline = FALSE,
                                         ...) {
  res <- purrr::imap_chr(
    .x = x,
    .f = function(wording, handle) {
      if (is.integer(handle)) {
        handle <- NULL  # do not print indeces as title
      }
      if (inline) {
        glue_collapse(
          x = c(
            glue("`{handle}`"),  # this goes to "" if there is name
            glue("^['{wording}']")
          )
        )
      } else {
        glue_collapse(
          x = c(
            glue("`{handle}`", ":    "),  # this goes to "" if there is name
            glue("> {wording}


                 ")  # looks ugly but adds the required newline at the end to get into the next environment
          ),
          sep = "\n \n"
        )
      }
    }
  )
  knitr::asis_output(res)
}


# subclass binary files ====
new_psItemContentBin <- function(items, dir_bin, geometry_opts) {
  new_psItemContent(
    items = items,
    dir_bin = dir_bin,
    geometry_opts = geometry_opts,
    subclass = "psItemContentBin"
  )
}

#' @describeIn psItemContent Validation
#' @noRd
#' @export
validate_S3.psItemContentBin <- function(x, ...) {
  dir_bin <- attr(x = x, which = "dir_bin")

  assert_directory_exists(x = dir_bin, access = "r", add = ps_coll)
  files <- file.path(dir_bin, as.vector(x))
  assert_file_exists(
    x = files,
    access = "r",
    .var.name = "file names",
    add = ps_coll)

  NextMethod(ps_coll = ps_coll)
}


# export method ====
#' @describeIn psItemContent Export rendered text items to pdf or svg.
#' @inheritParams export_ps
#' @inheritParams render_chain
#'
#' @section Rendering items:
#' It is often helpful to have a canonical, typeset version of text items, ready for for printing, web publishing or interpretation.
#' Rendered text items should meet several criteria:
#' - They should always fit on the *same* card size, both for practical reasons and to emphasize the equal "significance" of items, even if they are of different length.
#'     Card sizes can be arbitrary, specified via `paperwidth`,`paperheight`.
#' - They should always *look identical*, no matter where a participant or researcher encounters them.
#'     Even slight variations in, for example, line wrapping, might give an item a slightly different emphasis.
#' - Given the central status of text items in the methodology and the package, they should by *typeset professionally*.
#'
#' To meet these criteria, text items are
#' 1. converted to **LaTeX**, using [pandoc](https://pandoc.org), then
#' 2. compiled to **PDF**, using [LaTeX](https://www.latex-project.org) via [tinytex::latexmk()], then
#' 3. converted to **SVG**, using [pdf2svg](https://github.com/dawbarton/pdf2svg), then
#' 4. imported to **R Graphics** (grid graphics, to be precise) via [grImport2::readPicture()].
#'     Items are now fully available to the R Graphics system and can be used wherever [graphics::plot()] (or, to be precise, [grid::grid.draw()]) works.
#'
#' At each step of this necessary, but rather long conversion pipeline more (system) dependencies are required and asserted.
#' Additionally, because some of the intermediary formats cannot be easily or fully converted, downstream outputs may be faulty.
#'
#' Always use the *earliest* possible output from the above conversion pipeline to maximize fidelity to the original PDF.
# TODO mention that user-facing functions such as plot and knit_print automatically do this as far as possible.
# TODO explain how you can write stuff to disc
# TODO explain caching
#'
#' Because items always have to fit on one page, this function errors out when the rendered item would fill more than one page.
export_ps.psItemContentText <- function(x, dir = ".", overwrite = FALSE, format = "pdf") {
  assert_S3(x)

  # capture formatting options
  # lang <- attr(x = items, which = "lang")
  # geometry_opts <- attr(x = items, which = "geometry_opts")
  # alignment <- attr(x = items, which = "alignment")

  res <- list(foo = "foo", bar = "bar")  # placeholder

  purrr::imap_chr(
    .x = res,
    .f = function(x, y) {
      out_path <- fs::path(dir, y, ext = format)
      if (!overwrite) {
        assert_no_file(x = out_path)
      }
      readr::write_file(
        x = x,
        path = out_path,
        append = FALSE
      )
      out_path
    }
  )
}
