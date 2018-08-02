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
#' 2. compiled to **PDF**, using [LaTeX](https://www.latex-project.org) via [tools::texi2pdf], then
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
        tex <- purrr::imap(
          .x = as.list(items),
          .f = function(x, name, pb) {
            pb$tick(tokens = list(name = name))
            x <- wrap_in_latex_fontsize(tex = x, fontsize_local = fontsize_local)
            x <- wrap_in_latex_alignment(tex = x, alignment = alignment)
            capture_disc_output(fun = md2tex)(
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

#' @title Render markdown file to LaTeX file
#' @description Function calls pandoc with some options to convert markdown to LaTeX.
#' @param path `[character(1)]` to an input file *with or without extension*
#' @inheritDotParams declare_pandoc_geometry
#' @inheritParams declare_pandoc_var
#' @return `[character(1)]` path to latex file *with extension* invisibly.
#' @keywords internal
md2tex <- function(path,
                   fontsize_global = "10pt",
                   lang = NULL,
                   ...) {

  # input validation
  path_in <- fs::path_ext_set(path = path, ext = "md")  # ensures that input is always md
  assert_file_exists(x = path_in, access = "r", extension = "md")

  # check dependencies
  requireNamespace2(x = "processx")

  # check system dependencies
  assert_sysdep(x = "pandoc")

  path_out <- fs::path_ext_set(path = path_in, ext = "tex")

  # render file to latex
  res <- processx::run(
    command = "pandoc",
    args = c(
      "--from=markdown",  # this is pandoc's extended markdown
      "--to=latex",
      "--verbose",
      "--standalone",
      "--fail-if-warnings",

      # other latex options
      "--variable=pagestyle:empty",
      declare_pandoc_geometry(...),
      declare_pandoc_fontsize(fontsize_global = fontsize_global),

      # language
      declare_pandoc_lang(lang = lang),

      # output
      glue("--output={path_out}"),

      path_in # input, must be last
    ),
    error_on_status = FALSE,  # this does not give good error messages
    windows_hide_window = TRUE,
    echo = FALSE,
    echo_cmd = FALSE,
    spinner = FALSE,  # screws up progressbar
    timeout = 1  # this is just pandoc, should be very fast
  )
  if (res$timeout) {
    stop(glue("Pandoc timed out converting {path_in} to {path_out}."), call. = FALSE)
  }
  if (res$status != 0) {
    stop(glue("Pandoc failed on converting {path_in} to {path_out} with: {res$stderr}"), call. = FALSE)
  }
  invisible(path_out)
}

#' @title Write file input, read file output
#' @description Function operator to let functions with disk side effects accept R object as input, and return R object as output.
#' @param fun A function which accepts a file as an input and returns a file name as an output.
#' Helpful for debugging purposes.
#' @return modified function
#' @keywords internal
capture_disc_output <- function(fun) {
  # input validation
  assert_function(x = fun, null.ok = FALSE)

  force(fun)

  function(x, path_in, ...) {
    # input validation
    assert_character(x = x, any.missing = FALSE, null.ok = FALSE)
    assert_path_for_output(x = path_in, overwrite = TRUE)

    withr::local_file(file = path_in)
    readr::write_lines(x = x, path = path_in, append = FALSE)
    out_path <- fun(path_in, ...)
    res <- readr::read_lines(file = out_path)
    fs::file_delete(out_path)
    res
  }
}

#' @title Render LaTeX to PDF
#' @description In contrast to normal texi function, this returns the pdf as a raw vector
#' @param tex
#' `[character()]` giving tex string(s).
#' @noRd
#' @return `[raw()]` giving PDF.
texi2pdf2 <- function(tex) {
  requireNamespace2(x = "tools")
  requireNamespace2(x = "fs")
  requireNamespace2(x = "withr")

  # just write to tempdir
  withr::local_dir(new = tempdir())

  write(x = tex, file = "item.tex")
  # availability of tex will ideally be checked in platform dependent way by texi2pdf
  tools::texi2pdf(file = "item.tex", clean = TRUE, index = FALSE, quiet = TRUE)

  readBin(
    con = "item.pdf",
    what = "raw",
    n = fs::file_info("item.pdf")$size  # need to allocate size
  )
}

#' @title Convert PDF to SVG
#' @param pdf
#' `[raw()]` of PDF file.
#' @noRd
#' @return `[raw()]` of SVG file.
pdf2svg <- function(pdf) {
  # dependencies
  requireNamespace2(x = "fs")
  requireNamespace2(x = "withr")
  requireNamespace2(x = "tools")
  checkmate::assert_os(os = c("mac", "linux"))
  assert_sysdep(x = "pdf2svg")

  # write to tempdir only
  withr::local_dir(new = tempdir())
  writeBin(object = pdf, con = "item.pdf")

  system2(
    command = "pdf2svg",
    args = c("item.pdf",
             "item.svg",
             "1"), # only take 1st page
    stderr = "")

  readBin(
    con = "item.svg",
    what = "raw",
    n = fs::file_info("item.svg")$size  # need to allocate size
  )

}

#' @title Convert SVG to R graphics system plot
#' @param svg
#' `[character(1)]` giving path to an SVG file.
#' @noRd
#' @return a grid grob.
svg2grob <- function(svg) {
  # dependencies
  requireNamespace2(x = "grImport2")

  withr::local_dir(new = tempdir())
  writeBin(object = svg, con = "item.svg")

  pic <- grImport2::readPicture(file = "item.svg", warn = FALSE)
  grImport2::pictureGrob(picture = pic)
}


#' @title Check if pdf is only 1 page long
#' @description Items must never overflow 1 page.
#' @param x `[character(1)]` giving the path to a pdf file.
#' @noRd
check_pdf1page <- function(x) {
  requireNamespace2(x = "pdftools")
  assert_file_exists(access = x)
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


# formatting helpers ====

# formatting helpers: pandoc opts ====
#' @title Make pandoc tex variable option
#' @description This function creates pandoc variable key value pairs for LaTeX preamble.
#' @param key `[character(1)]` giving the key, such as `"geometry"`.
#' @param value `[character(1)]` giving the value, such as `"margin=1in"`.
#' @keywords internal
#' @return `[character()]` giving pandoc variable option, option*s* in the case of geometry.
declare_pandoc_var <- function(key, value) {
  checkmate::assert_string(x = key, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_string(x = value, na.ok = FALSE, null.ok = FALSE)
  glue::glue("--variable={key}:{value}")
}

#' @describeIn declare_pandoc_var declare *base* font size
#' @eval document_choice_arg(arg_name = "fontsize_global", choices = fontsizes_global, before = "giving the document-wide font size.", default = "10pt")
declare_pandoc_fontsize <- function(fontsize_global = "10pt") {
  checkmate::assert_string(x = fontsize_global, na.ok = FALSE, null.ok = FALSE)
  checkmate::assert_choice(x = fontsize_global, choices = fontsizes_global, null.ok = FALSE)
  declare_pandoc_var(key = "fontsize", value = fontsize_global)
}
fontsizes_global <- c(
  # only these are permissible in base classes https://texblog.org/2012/08/29/changing-the-font-size-in-latex/
  # must remain in ascending order!
  "10pt",
  "11pt",
  "12pt"
)

#' @describeIn declare_pandoc_var declare language
#' @eval document_choice_arg(arg_name = "lang", choices = langs, before = "giving a [valid BCP 47 language code](https://tools.ietf.org/html/bcp47) code, such as `en_US`.", after = "Used for multilingual typsetting support via [LaTeX's babel package](https://ctan.org/pkg/babel) and others.", null = "in which case there is no multilingual support", default = "null")
declare_pandoc_lang <- function(lang = NULL) {
  assert_choice(x = lang, choices = langs, null.ok = TRUE)
  if (!is.null(lang)) {
    declare_pandoc_var(key = "lang", value = lang)
  }
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
# must be converted
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


#' @describeIn declare_pandoc_var declare options for [LaTeX geometry package](https://ctan.org/pkg/geometry).
#'
#' @param paperwidth,paperheight
#' `[numeric(1)]` giving the width and height of documents in `unit`.
#' For good typographical results, should be as close as possible to the *actual* physical measurements of cards encountered by users.
#' Defaults to `8.5` width and `5.4`. height.
#'
#' @param top,bottom,left,right
#' `[numeric(1)]` giving the margin in `unit`.
#' Defaults to `0.5`.
#'
#' @eval document_choice_arg(arg_name = "unit", choices = units, before = "giving the units for the above dimensions.", default = "cm")
#'
#' @param vcentering
#' `[logical(1)]` indicating whether content should be vertically centered.
#' Defaults to `TRUE`.
#' @param hcentering
#' `[logical(1)]` indicating whether content should be horizontally centered.
#' Defaults to `TRUE`.
declare_pandoc_geometry <- function(paperwidth = 8.5,
                                    paperheight = 5.4,
                                    top = 0.5,
                                    bottom = 0.5,
                                    left = 0.5,
                                    right = 0.5,
                                    unit = "cm",
                                    vcentering = TRUE,
                                    hcentering = TRUE) {
  assert_string(x = unit, na.ok = FALSE, null.ok = FALSE)
  assert_choice(x = unit, choices = units)
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
  opts <- purrr::imap_chr(
    .x = num_arguments,
    .f = function(x, y) {
      # this is input validation
      assert_numeric(x = x, lower = 0, finite = TRUE, any.missing = FALSE, len = 1, null.ok = FALSE, .var.name = y)

      # this is the actual paste job
      value = glue::glue("{y}={x}{unit}")
    }
  )
  # append v/hcentering if applicable
  # this is a bit confusing because those are really just pasted as options, they are *not* themselves key/value pairs (as the above are)
  if (vcentering) {
    opts <- c(opts, vcentering = "vcentering")
  }
  if (hcentering) {
    opts <-  c(opts, hcentering = "hcentering")
  }

  opts <- purrr::map_chr(.x = opts, .f = function(x) {
    declare_pandoc_var(key = "geometry", value = x)
  })
  return(opts)
}
units <- c(metric = "cm", imperial = "in")


# formatting helpers: latex wrapers ====
#' @title Wrap latex string in latex environment
#' @description These are helper functions to apply latex environments.
#' @param env `[character(1)]` giving a latex environment.
#' @param tex `[character(1)]` giving some latex string.
#' @return `[character(1)]` a latex string
#' @keywords internal
wrap_in_latex_env <- function(env, tex) {
  assert_string(x = tex, min.chars = 1, na.ok = FALSE, null.ok = FALSE)
  assert_string(x = env, min.chars = 1, na.ok = FALSE, null.ok = FALSE)
  glue::glue(
    "\\begin{[env]}
    [tex]
    \\end{[env]}",
    .open = "[",
    .close = "]"
  )
}

#' @describeIn wrap_in_latex_env Apply local fontsize
#' @eval document_choice_arg(arg_name = "fontsize_local", choices = fontsizes_local, before = "giving a valid [LaTeX font size](https://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text).", default = "tiny")
wrap_in_latex_fontsize <- function(fontsize_local = "tiny", tex) {
  assert_choice(x = fontsize_local, choices = fontsizes_local, null.ok = FALSE)
  wrap_in_latex_env(env = fontsize_local, tex = tex)
}
fontsizes_local <- c(
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

#' @describeIn wrap_in_latex_env Apply alignment
#' @eval document_choice_arg(arg_name = "alignment", choices = alignments, before = "giving the alignment of the text.", default = "justified")
wrap_in_latex_alignment <- function(alignment = "justified", tex) {
  assert_choice(x = alignment, choices = alignments, null.ok = FALSE)
  if (alignment == "justified") {
    # if null, the justified, which requires NO extra command
    return(tex)
  }
  env <- switch(
    EXPR = alignment,
    left = "flushleft",
    right = "flushright",
    center = "center"
  )
  wrap_in_latex_env(env = env, tex = tex)
}
alignments <- c(
  # this list is from https://www.sharelatex.com/learn/Text_alignment
  # we're only using vanilla latex, no extra package
  "justified",
  "left",
  "right",
  "center"
)
