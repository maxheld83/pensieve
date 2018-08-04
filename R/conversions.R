# format2format ====

#' @name format2format
#' @title Convert vector formats
#' @description Call various tools with options to convert vector formats.
#' @details
#' Functions with the `_mem` postfix abstract away file system operations and have been cached via [memoise::memoise()].
#' @param path `[character(1)]` to an input file *with or without extension*
#' @return depending on the function postfix:
#' - for the base functions, `[character(1)]` (invisibly) giving path to output file *with extension*.
#'   **Exception**: [svg2grob()] always returns a grid grob.
#' @keywords internal
NULL

#' @inheritDotParams declare_pandoc_geometry
#' @inheritParams declare_pandoc_var
#' @describeIn format2format markdown to LaTeX via [pandoc](http://pandoc.org)
md2tex <- function(path,
                   fontsize_global = "10pt",
                   lang = NULL,
                   ...) {

  path_in <- set_proper_extension(path = path, ext = "md")

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

#' @describeIn format2format latex to pdf via [LaTeX](https://www.latex-project.org)
texi2pdf2 <- function(path) {
  # input validation
  path_in <- set_proper_extension(path = path, ext = "tex")

  # dependencies
  requireNamespace2("fs")
  requireNamespace2("tinytex")

  # this also downloads LaTeX packages as far as possible
  invisible(tinytex::latexmk(file = path_in, engine = "pdflatex", install_packages = TRUE, clean = TRUE, max_times = 2))
}

#' @describeIn format2format PDF to SVG via [pdf2svg](http://www.cityinthesky.co.uk/opensource/pdf2svg/)
#' @param page `[integer(1)]` giving the page in the pdf to convert.
pdf2svg <- function(path, page = 1) {
  # input validation
  path_in <- set_proper_extension(path = path, ext = "pdf")

  # dependencies
  requireNamespace2(x = "fs")
  requireNamespace2(x = "processx")
  checkmate::assert_os(os = c("mac", "linux"))

  # sysdeps
  assert_sysdep(x = "pdf2svg")

  path_out <- fs::path_ext_set(path = path, ext = "svg")

  res <- processx::run(
    command = "pdf2svg",
    args = c(
      path_in,
      path_out,
      page
    ),
    error_on_status = FALSE,  # this does not give good error messages
    windows_hide_window = TRUE,
    echo = FALSE,
    echo_cmd = FALSE,
    spinner = FALSE,  # screws up progressbar
    timeout = 5  # this might take a while
  )

  if (res$timeout) {
    stop(glue("pdf2svg timed out converting {path_in} to {path_out}."), call. = FALSE)
  }
  if (res$status != 0) {
    stop(glue("pdf2svg failed on converting {path_in} to {path_out} with: {res$stderr}"), call. = FALSE)
  }
  invisible(path_out)
}

#' @describeIn format2format SVG to R graphics (grid) via [grImport2::readPicture()]
svg2grob <- function(path) {
  # input validation
  path_in <- set_proper_extension(path = path, ext = "svg")

  # dependencies
  requireNamespace2(x = "grImport2")

  pic <- grImport2::readPicture(file = path_in, warn = FALSE)
  grImport2::pictureGrob(picture = pic)
}

#' @title If necessary, append extension to path
#' @description This returns the good filename *and* changes the file on disc (side effect).
#' @inheritParams format2format
#' @return `[character(1)]` giving path to proper file name
#' @noRd
set_proper_extension <- function(path, ext) {
  requireNamespace2("fs")
  assert_file_exists(x = path, access = "r")
  path_proper <- fs::path_ext_set(path = path, ext = ext)  # ensures that input is always proper
  fs::file_move(path = path, new_path = path_proper)
  path_proper
}


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
#' @eval document_choice_arg(arg_name = "lang", choices = langs, before = "giving a [valid BCP 47 language code](https://tools.ietf.org/html/bcp47) code, such as `en_US`.", after = "Used for multilingual typsetting support via [LaTeX's babel package](https://ctan.org/pkg/babel) and others. **Careful**: Depending on the local tex distribution, not all valid languages may also be supported by LaTeX. Use [check_latex_lang()] to verify.", null = "in which case there is no multilingual support", default = "null")
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


# predicates ====

#' @title Check if pdf is only 1 page long
#' @description Checks if pdf is longer than 1 page.
#' @param x `[character(1)]` giving the path to a pdf file.
#' @noRd
check_pdf1page <- function(x) {
  requireNamespace2(x = "pdftools")
  assert_file_exists(x = x, access = "r")
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


#' @title Check if language can be compiled
#' @description Checks if pandoc language can be compiled given local tex distribution.
#' @param x `[character(1)` giving a pandoc language, same as `lang` in [md2tex()].
#' @inheritParams checkmate::check_vector
#' @inherit checkmate::check_vector return
#' @keywords internal
check_latex_lang <- function(x) {
  assert_choice(x = x, choices = langs, null.ok = FALSE)

  requireNamespace2(x = "fs")

  tmpdir <- fs::path_temp()

  path_in <- fs::path(tmpdir, "bad_lang", ext = "md")
  readr::write_file(
    x = "Because of a bad language, I will never be a PDF.",
    path = path_in,
    append = FALSE
  )
  path_in <- md2tex(path = path_in, lang = x)

  res <- TRUE
  res <- tryCatch(
    expr = {
      suppressMessages(texi2pdf2(path = path_in))
      TRUE
    },
    # unfortunately, above code will return a path
    warning = function(cnd) {
      bad_lang <- stringr::str_detect(string = conditionMessage(cnd), pattern = ".ldf")
      if (bad_lang) {
        glue("LaTeX is unable to compile with language {x}: conditionMessage(cnd)")
      } else {
        # might have other error messages, but then it's NOT clearly the lang, so we stick to true
        TRUE
      }
    },
    error = function(cnd) {
      glue("LaTeX seems unable to compile with language {x} for an unknown error message: {conditionMessage(cnd)}")
    }
  )

  res
}
#' @rdname check_latex_lang
expect_latex_lang <- checkmate::makeExpectationFunction(check.fun = check_latex_lang)
#' @rdname check_latex_lang
test_latex_lang <- checkmate::makeTestFunction(check.fun = check_latex_lang)
#' @rdname check_latex_lang
assert_latex_lang <- checkmate::makeAssertionFunction(check.fun = check_latex_lang)

# FOs ====

#' @title Write file input, read file output
#' @description Function operator to let functions with disk side effects accept R object as input, and return R object as output.
#' @param fun A function which accepts a file as an input and returns a file name as an output.
#' Helpful for debugging purposes.
#' @return modified function
#' @keywords internal
virtually <- function(fun) {
  # input validation
  assert_function(x = fun, null.ok = FALSE)

  force(fun)

  function(x, path_in = "foo", ...) {
    # input validation
    assert_vector(x = x, any.missing = FALSE, null.ok = FALSE)
    # might be binary, so we can't test for more
    assert_path_for_output(x = path_in, overwrite = TRUE)

    # dependencies
    requireNamespace2(x = "withr")
    requireNamespace2(x = "fs")

    withr::local_file(file = path_in)
    if (is.raw(x)) {
      writeBin(object = x, con = path_in)
    } else {
      readr::write_lines(x = x, path = path_in, append = FALSE)
    }

    path_out <- fun(path_in, ...)

    if (is_binary(path_out)) {
      res <- readr::read_file_raw(file = path_out)
    } else {
      res <- readr::read_lines(file = path_out)
    }

    fs::file_delete(path_out)
    res
  }
}
#' @title Memoise a function if available
#' @description Memoises a function if [memoise::memoise()] is available.
#' @param f Function of which to create a memoised copy.
#' @noRd
memoise2 <- function(f) {
  # input validation
  assert_function(x = f, null.ok = FALSE)
  if (requireNamespace("memoise")) {
    return(memoise::memoise(f = f))
  } else {
    message("This function runs slow because package memoise is missing. Please install it.")
    f
  }
}
# sadly, these have to be down here, *after* virtually, otherwise won't work
#' @describeIn format2format markdown to LaTeX via [pandoc](http://pandoc.org)
#' @param x `[character()]` *or* `[raw()]` giving the input.
#' @param path_in `[character(1)]` giving path to use for input file *with or without extension*.
#' Defaults to `"foo"`.
#' Useful for debugging.
#' @return
#' - For `_mem`, `[character()]` or `[raw()]`.
md2tex_mem <- memoise2(virtually(fun = md2tex))
#' @describeIn format2format latex to pdf via [LaTeX](https://www.latex-project.org)
texi2pdf2_mem <- memoise2(virtually(fun = texi2pdf2))
#' @describeIn format2format PDF to SVG via [pdf2svg](http://www.cityinthesky.co.uk/opensource/pdf2svg/)
pdf2svg_mem <- memoise2(virtually(fun = pdf2svg))
#' @describeIn format2format SVG to R graphics (grid) via [grImport2::readPicture()]
svg2grob_mem <- memoise2(function(x, path_in = "foo") {
  # input validation
  assert_vector(x = x, any.missing = FALSE, null.ok = FALSE)
  assert_path_for_output(x = path_in, overwrite = TRUE)

  # dependencies
  requireNamespace2(x = "withr")

  withr::local_file(file = path_in)
  writeBin(object = x, con = path_in)

  svg2grob(path = path_in)
})

#' @title Test if path is to a binary file
#' @description Tests if a path is part of some known text files, otherwise binary
#' @param path `[character(1)]` giving path to a file
#' @noRd
is_binary <- function(path) {
  requireNamespace2(x = "fs")
  !fs::path_ext(path) %in% c("md", "tex", "txt")
}

#' @title Find largest possible fontsize given all other arguments
#' @description Finds largest possible fontsize for some markdown to fit on one PDF page.
#' @param fontsizes_global_possible `[character()]` giving possible fontsizes_local
#' @inheritDotParams md2tex -fontsize_global
#' @return `[character(1)]` giving largest possible fontsize
#' @keywords internal
find_max_fontsize <- function(fontsizes_global_possible = fontsizes_global, ...) {
  assert_subset(x = fontsizes_global_possible, choices = fontsizes_global)

  working_fontsizes <- purrr::map_lgl(.x = fontsizes_global_possible, .f = function(this_size) {
    tex <- md2tex_mem(fontsize_global = this_size, ...)
    pdf <- texi2pdf2_mem(x = tex)
    out_path <- fs::path("test_fontsize", ext = "pdf")
    withr::local_file(file = out_path)
    writeBin(object = pdf, con = out_path)
    test_pdf1page(x = out_path)
  })

  if (!(any(working_fontsizes))) {
    stop(
      "Could not find a fontsize to fit the text on one page given the other arguments. ",
      "Try shortening the text or using other additional arguments which take up less space.",
      call. = FALSE
    )
  }

  fontsizes_global_possible[max(which(working_fontsizes))]
}

