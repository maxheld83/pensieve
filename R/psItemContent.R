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
# TODO this is a problem, because item handles should be in a different column :( of psItems, unclear!
#'
#' @param type
#' `[character(1)]` giving the *kind* of item content, must be one of:
#' - `"text"` (default) for textual item, in which case `items` must be text.
#'   Items can be marked up using [Pandoc Markdown](https://rmarkdown.rstudio.com/authoring_pandoc_markdown.html).
#'   An additional subclass `psItemContentText` is prepended and validated.
#' - `"image"` for image items, in which case `items` must be file paths, relative from `img_dir`.
#'   Images must be `*.png`, `*.jpg`, `*.jpeg` or `*.svg`.
#'   An additional subclass `psItemContentImage`` is prepended and validated.
#'   `lang` is ignored.
#'
#' @param img_dir
#' `[character(1)]` giving the directory for `type = "image"`s.
#' Defaults to `NULL`, in which case images are expected at the working directory root [base::getwd()].
#' Ignored unless `type = "image"`.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#'
#' @inheritParams declare_pandoc_lang
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
#' @title
#' Render text items.
#'
#' @description
#' Renders character vectors of items to pdf, svg and R graphics (grid).
#'
#' @details
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
#'
#' Additionally, because some of the intermediary formats cannot be easily or fully converted, downstream outputs may be faulty.
#' Always use the *earliest* possible output from the above conversion pipeline to maximize fidelity to the original PDF.
# TODO mention that user-facing functions such as plot and knit_print automatically do this as far as possible.
# TODO explain how you can write stuff to disc
# TODO explain caching
#'
#' When `items` are named with item handles, such handles are used as file and object names.
#'
#' Because items always have to fit on one page, this function errors out when the rendered item would fill more than one page.
#'
#' @inheritParams psItemContent
#' @inheritParams md2tex
#' @inheritParams wrap_in_latex_env
#' @inheritParams declare_pandoc_geometry
#'
#' @param tex `[list(character())]` giving a list of manually produced LaTeX markup, one for each `items`.
#' Defaults to `NULL`, in which case the LaTeX markup is rendered automatically (recommended).
#'
#' @param pdf `[list(raw())]` giving a list of manually produced PDFs, one for each `items`.
#' Defaults to `NULL`, in which case the PDF is rendered automatically (recommended).
#'
#' @param svg `[list(raw())]` giving a list of manually produced SVGs, one for each `items`.
#' Defaults to `NULL`, in which case the SVG is rendered automatically (recommended).
#'
#' @param grob `[list(grob())]` giving a list of manually produced grobs, one for each `items`.
#' Defaults to `NULL`, in which case the grob is rendered automatically (recommended).
#'
#' @export
render_items <- function(items,
                         fontsize_local = "tiny",
                         fontsize_global = "10pt",
                         alignment = "justified",
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
                         tex = NULL,
                         pdf = NULL,
                         svg = NULL,
                         grob = NULL) {

  # do items coercion here

  # input validation
  purrr::walk(.x = list(tex = tex, pdf = pdf, svg = svg, grob = grob), .f = function(x) {
    assert_list(x = x, any.missing = FALSE, unique = TRUE, null.ok = TRUE, len = length(items))
  })
  purrr::walk(.x = tex, .f = function(x) {
    assert_character(x = x, any.missing = FALSE, null.ok = TRUE)
  })
  purrr::walk(.x = pdf, .f = function(x) {
    if (!is.null(x)) assertTRUE(x = is.raw(x))
  })
  purrr::walk(.x = svg, .f = function(x) {
    if (!is.null(x)) assertTRUE(x = is.raw(x))
  })
  purrr::walk(.x = grob, .f = function(x) {
    if (!is.null(x)) assertTRUE(x = grid::is.grob(x))
  })

  # check dependencies
  requireNamespace2(x = "progress")

  # md2tex ====
  # below function requires these as a list
  if (is.null(tex)) {
    message("Step 1/4: Conversion from Markdown to LaTeX.")
    tryCatch(
      expr = {
        # set up progress bar
        pb <- progress::progress_bar$new(
          total = 100,
          format = "converting :name [:bar] :percent eta: :eta")
        pb$tick(0) # start with 0 before first compute
        # notice that, strictly speaking, this needs to run *all* fontsizes, because it's possible (given latex complexity) that, say fontsize 1 works, 2 fails and 3 works again
        # could happen because of other latex optimisations
        # so we're not saving runs here, because that might end up being only a local optimum
        tex <- purrr::imap(
          .x = as.list(items),
          .f = function(x, name, pb) {
            pb$tick(tokens = list(name = name))
            x <- wrap_in_latex_fontsize(tex = x, fontsize_local = fontsize_local)
            x <- wrap_in_latex_alignment(tex = x, alignment = alignment)
            md2tex_mem(
              x = x,
              path_in = fs::path_ext_set(path = name, ext = "md"),
              fontsize_global = fontsize_global,
              lang = lang
            )
          },
          pb
        )
        pb$terminate()
      },
      error = function(cnd) {
        tex <- NULL  # if one compilation fails, set all to NULL
        warning(
          glue::glue("Skipping conversion to LaTeX: ", conditionMessage(cnd)),
          call. = FALSE
        )
      }
    )
  } else {
    message("Skipping conversion to LaTeX: Using user-supplied LaTeX markup from 'tex'.")
  }

  list(tex = as.list(tex))
}
