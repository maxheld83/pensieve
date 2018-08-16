# markup2vector ====

#' @name markup2vector
#' @title Convert markup to vector formats
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
#' @describeIn markup2vector markdown to LaTeX via [pandoc](http://pandoc.org)
md2tex <- function(path,
                   fontsize_global = NULL,
                   linestretch = NULL,
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
      declare_pandoc_linestretch(linestretch = linestretch),

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
    timeout = 100  # this is just pandoc, should be very fast
  )
  if (res$timeout) {
    stop(
      glue("Pandoc timed out converting {path_in} to {path_out}."),
      call. = FALSE
    )
  }
  if (res$status != 0) {
    stop(
      glue("Pandoc failed on converting {path_in} to {path_out} with: {res$stderr}"),
      call. = FALSE
    )
  }
  invisible(path_out)
}

#' @describeIn markup2vector latex to pdf via [LaTeX](https://www.latex-project.org)
texi2pdf2 <- function(path) {
  # input validation
  path_in <- set_proper_extension(path = path, ext = "tex")

  # dependencies
  requireNamespace2("tinytex")

  # this also downloads LaTeX packages as far as possible
  invisible(
    tinytex::latexmk(
      file = path_in,
      engine = "pdflatex",
      install_packages = TRUE,
      clean = TRUE,
      max_times = 2
    )
  )
}

#' @describeIn markup2vector PDF to SVG via [pdf2svg](http://www.cityinthesky.co.uk/opensource/pdf2svg/)
#' @param page `[integer(1)]` giving the page in the pdf to convert.
pdf2svg <- function(path, page = 1) {
  # input validation
  path_in <- set_proper_extension(path = path, ext = "pdf")

  # dependencies
  requireNamespace2(x = "fs")
  requireNamespace2(x = "processx")
  assert_os(os = c("mac", "linux"))

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
    stop(
      glue("pdf2svg timed out converting {path_in} to {path_out}."),
      call. = FALSE
    )
  }
  if (res$status != 0) {
    stop(
      glue("pdf2svg failed on converting {path_in} to {path_out} with: {res$stderr}"),
      call. = FALSE
    )
  }
  invisible(path_out)
}

#' @describeIn markup2vector SVG to R graphics (grid) via [grImport2::readPicture()]
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
#' @inheritParams markup2vector
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
  assert_string(x = key, na.ok = FALSE, null.ok = FALSE)
  assert_vector(x = value, any.missing = FALSE, null.ok = TRUE)
  if (is.null(value)) {
    return(character(0))  # to streamline output with other functions; this is the purrr logic type stability
  } else {
    glue("--variable={key}:{value}")
  }
}

#' @describeIn declare_pandoc_var declare *base* font size
#' @eval document_choice_arg(arg_name = "fontsize_global", choices = fontsizes_global, before = "giving the document-wide font size.", default = "null", null = "in which case the system default fontsize is used.")
declare_pandoc_fontsize <- function(fontsize_global = NULL) {
  assert_choice(x = fontsize_global, choices = fontsizes_global, null.ok = TRUE)
  declare_pandoc_var(key = "fontsize", value = fontsize_global)
}
fontsizes_global <- c(
  # only these are permissible in base classes https://texblog.org/2012/08/29/changing-the-font-size-in-latex/
  # must remain in ascending order!
  "10pt",
  "11pt",
  "12pt"
)

#' @describeIn declare_pandoc_var declare `linestretch` (to be passed on to LaTeX [setspace](https://ctan.org/pkg/setspace) package)
#' @param linestretch `[numeric()]`
#' giving the line spacing in multiples, e.g. `1.25`, `1.5`.
#' Defaults to `NULL` for default LaTeX line spacing.
declare_pandoc_linestretch <- function(linestretch = NULL) {
  assert_numeric(x = linestretch, lower = 1, finite = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE)
  declare_pandoc_var(key = "linestretch", value = linestretch)
}

#' @describeIn declare_pandoc_var declare language
#' @eval document_choice_arg(arg_name = "lang", choices = langs, before = "giving a [valid BCP 47 language code](https://tools.ietf.org/html/bcp47) code, such as `en_US`.", after = "Used for multilingual typsetting support via [LaTeX's babel package](https://ctan.org/pkg/babel) and others. **Careful**: Depending on the local tex distribution, not all valid languages may also be supported by LaTeX. Use [check_latex_lang()] to verify.", null = "in which case there is no multilingual support", default = "null")
declare_pandoc_lang <- function(lang = NULL) {
  assert_choice(x = lang, choices = langs, null.ok = TRUE)
  declare_pandoc_var(key = "lang", value = lang)
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
langs <- pmap(
  .l = langs[,c("lang_short", "var_short", "lang_long", "var_long")],
  .f = function(lang_short, var_short, lang_long, var_long) {
    if (is.na(var_short)) {
      short <- lang_short
    } else {
      short <- glue('{lang_short}-{var_short}')
    }
    if (is.na(var_long)) {
      long <- lang_long
    } else {
      long <- glue('{lang_long} ({var_long})')
    }
    names(short) <- long
    return(short)
  }
)
langs <- as_vector(langs)


#' @describeIn declare_pandoc_var declare options for [LaTeX geometry package](https://ctan.org/pkg/geometry).
#'
#' @param paperwidth,paperheight
#' `[numeric(1)]` giving the width and height of documents in `unit`.
#' For good typographical results, should be as close as possible to the *actual* physical measurements of documents encountered by users.
#' Defaults to `NULL`.
#'
#' @param top,bottom,left,right
#' `[numeric(1)]` giving the margin in `unit`.
#' Defaults to `NULL`.
#'
#' @eval document_choice_arg(arg_name = "unit", choices = units, before = "giving the units for the above dimensions.", default = "cm")
#'
#' @param vcentering,hcentering
#' `[logical(1)]` indicating whether content should be vertically/horizontally centered.
#' Defaults to `FALSE`.
declare_pandoc_geometry <- function(paperwidth = NULL,
                                    paperheight = NULL,
                                    top = NULL,
                                    bottom = NULL,
                                    left = NULL,
                                    right = NULL,
                                    unit = "cm",
                                    vcentering = FALSE,
                                    hcentering = FALSE) {
  assert_choice(x = unit, choices = units, null.ok = FALSE)
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
  # keep only actually filled options
  num_arguments <- discard(.x = num_arguments, .p = is.null)

  opts <- imap_chr(
    .x = num_arguments,
    .f = function(x, y) {
      # this is input validation
      assert_numeric(x = x, lower = 0, finite = TRUE, any.missing = FALSE, len = 1, null.ok = TRUE, .var.name = y)
      value = glue("{y}={x}{unit}")
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

  map_chr(.x = opts, .f = function(x) {
    declare_pandoc_var(key = "geometry", value = x)
  })
}
units <- c(metric = "cm", imperial = "in")


# formatting helpers: latex wrapers ====
#' @title Wrap character vector in latex environment
#' @description These are helper functions to apply latex environments.
#' @param tex `[character()]` giving some character vector.
#' @param env `[character(1)]` giving a latex environment, defaults to `NULL` for no latex environment.
#' @return `[character()]` a character vector of valid latex
#' @keywords internal
wrap_in_latex_env <- function(tex, env = NULL) {
  assert_character(x = tex, any.missing = FALSE, null.ok = FALSE)
  assert_string(x = env, min.chars = 1, na.ok = FALSE, null.ok = TRUE)
  if (is.null(env)) {
    return(tex)
  }
  c(
    glue("\\begin{[env]}", .open = "[", .close = "]"),
    tex,
    glue("\\end{[env]}", .open = "[", .close = "]")
  )
}

#' @describeIn wrap_in_latex_env Apply local fontsize
#' @eval document_choice_arg(arg_name = "fontsize_local", choices = fontsizes_local, before = "giving a valid [LaTeX font size](https://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text).", null = "in which case the default local fontsize is used", default = "null")
wrap_in_latex_fontsize <- function(tex, fontsize_local = NULL) {
  assert_choice(x = fontsize_local, choices = fontsizes_local, null.ok = TRUE)
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
wrap_in_latex_alignment <- function(tex, alignment = "justified") {
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
assert_pdf1page <- makeAssertionFunction(check.fun = check_pdf1page)
test_pdf1page <- makeTestFunction(check.fun = check_pdf1page)
expect_pdf1page <- makeExpectationFunction(check.fun = check_pdf1page)


#' @title Check if language can be compiled
#' @description Checks if pandoc language can be compiled given local tex distribution.
#' @param x `[character(1)` giving a pandoc language, same as `lang` in [md2tex()].
#' @inheritParams checkmate::check_vector
#' @inherit checkmate::check_vector return
#' @keywords internal
check_latex_lang <- function(x) {
  assert_choice(x = x, choices = langs, null.ok = FALSE)

  res <- TRUE
  res <- tryCatch(
    expr = {
      tex <- md2tex_mem(
        x = "Because of a bad language, I will never be a PDF.",
        lang = x
      )
      # watch out: texi2pdf2 can be abstracted, but to be safe, should not be memoised
      # if memoised, we might get an old (from old cache) value, which is not invalidated (of course) on tex distro changes
      suppressMessages(virtually(texi2pdf2)(tex))
      TRUE
    },
    # unfortunately, above code will return a path
    warning = function(cnd) {
      bad_lang <- stringr::str_detect(
        string = conditionMessage(cnd),
        pattern = ".ldf"
      )
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
expect_latex_lang <- makeExpectationFunction(check.fun = check_latex_lang)
#' @rdname check_latex_lang
test_latex_lang <- makeTestFunction(check.fun = check_latex_lang)
#' @rdname check_latex_lang
assert_latex_lang <- makeAssertionFunction(check.fun = check_latex_lang)


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

    # dependencies
    requireNamespace2(x = "withr")
    requireNamespace2(x = "fs")

    tmpdir <- fs::path_temp()
    withr::local_dir(new = tmpdir)
    if (is.raw(x)) {
      readr::write_file(x = x, path = path_in, append = FALSE)
    } else {
      readr::write_lines(x = x, path = path_in, append = FALSE)
    }

    path_out <- fun(path_in, ...)

    if (grid::is.grob(path_out)) {
      # mildly dirty hack follows.
      # svg2grob is slightly different, because it returns an R object, not a path to a file
      res <- path_out
    } else if (is_binary(path_out)) {
      res <- readr::read_file_raw(file = path_out)
    } else {
      res <- readr::read_lines(file = path_out)
    }

    # cleanup, even if its just a temp folder
    fs::file_delete(path = fs::dir_ls(path = ".", regexp = path_in, recursive = FALSE, all = TRUE, fail = FALSE))
    res
  }
}


# sadly, these have to be down here, *after* virtually, otherwise won't work
#' @describeIn markup2vector markdown to LaTeX via [pandoc](http://pandoc.org)
#' @param x `[character()]` *or* `[raw()]` giving the input.
#' @param path_in `[character(1)]` giving path to use for input file *with or without extension*.
#' Defaults to `"foo"`.
#' Useful for debugging.
#' @inheritParams wrap_in_latex_env
#' @return
#' - For `_mem`, `[character()]` or `[raw()]`.
md2tex_mem <- memoise::memoise(
  function(x, path_in = "foo", fontsize_local = NULL, alignment = "justified", ...) {
  # latex wrapping is only available here in the virtualized variant;
  # because wrapping latex on filesystem would be too awkward/cumbersome
  x <- wrap_in_latex_fontsize(fontsize_local = fontsize_local, tex = x)
  x <- wrap_in_latex_alignment(alignment = alignment, tex = x)
  virtually(fun = md2tex)(x = x, path_in = path_in, ...)
  }
)
#' @describeIn markup2vector latex to pdf via [LaTeX](https://www.latex-project.org)
texi2pdf2_mem <- memoise::memoise(virtually(fun = texi2pdf2))
#' @describeIn markup2vector PDF to SVG via [pdf2svg](http://www.cityinthesky.co.uk/opensource/pdf2svg/)
pdf2svg_mem <- memoise::memoise(virtually(fun = pdf2svg))
#' @describeIn markup2vector SVG to R graphics (grid) via [grImport2::readPicture()]
svg2grob_mem <- memoise::memoise(virtually(fun = svg2grob))


#' @title Test if path is to a binary file
#' @description Tests if a path is part of some known text files, otherwise binary
#' @param path `[character(1)]` giving path to a file
#' @noRd
is_binary <- function(path) {
  requireNamespace2(x = "fs")
  !fs::path_ext(path) %in% c("md", "tex", "txt")
}


# rendering chain ====
#' @title
#' Render a list of markdown vectors to a desired format.
#' @description
#' Goes through the conversion chain as long as necessary to return the desired output.
#' @param l `[list()]`
#' giving `x`s to be passed to [md2tex_mem()].
#' @eval document_choice_arg(arg_name = "format", choices = names(render_chain_formats), before = "giving the output format to render items in.", default = "pdf")
#' @param ... arguments passed on to downstream formatting functions
#' @keywords internal
#' @return `[list()]`
#' of output format objects.
render_chain <- function(l,
                         format,
                         ...) {
  assert_list(x = l, types = "character", any.missing = FALSE, null.ok = FALSE)
  assert_choice(x = format, choices = names(render_chain_formats))
  requireNamespace2("progress")

  # how many steps need to be done?
  n_steps <- which(names(render_chain_formats) == format)

  pb <- progress::progress_bar$new(
    total = n_steps * length(l),
    format = "Rendering element :name, step :step/:n_steps [:bar] :percent eta: :eta"
  )

  pb$tick(0)  # start with 0 before first compute

  imap(
    .x = l,
    .f = function(content, name) {
      name <- as.character(name)  # to protect against imap integers from unnamed list elementsL
      res <- content
      step <- 1
      while (step <= n_steps) {
        pb$tick(1, tokens = list(name = name, step = step, n_steps = n_steps))
        if (step == 1) {
          # step 1 requires other arguments
          res <- invoke(.f = render_chain_formats[[step]], .x = list(x = res, path_in = name), ...)
        } else {
          res <- invoke(.f = render_chain_formats[[step]], .x = list(x = res, path_in = name))
        }
        step <- step + 1
      }
      return(res)
    }
  )
}
render_chain_formats <- list(
  tex = md2tex_mem,
  pdf = texi2pdf2_mem,
  svg = pdf2svg_mem,
  grob = svg2grob_mem)
# these must stay in the order of the conversion chain!


#' @title Find largest possible fontsize given all other arguments
#' @description Finds largest possible fontsize for list of markdown vectors to fit on one PDF page.
#' @inheritParams render_chain
#' @param fontsizes_local_possible `[character()]` giving possible fontsizes_local, defaults to all allowed values as per [md2tex_mem()].
#' @return `[character(1)]` giving largest possible fontsize
#' @keywords internal
find_fontsize <- function(l, fontsizes_local_possible = fontsizes_local, ...) {
  assert_list(x = l, types = "character", any.missing = FALSE, null.ok = FALSE)
  requireNamespace2(x = "progress")
  # TODO purr will support progress bars at some point, streamline this then
  # because below reduce has an init, the first go (and maybe more?) are not really registered by the progress bar, until LaTeX returns control
  # as a result, the progress bar takes very long to show up
  # to hack-fix this, we can "force" a show_after 0
  if (TRUE) {
    show_after <- 0
  } else {
    show_after <- 2/10  # pkg default
  }
  pb <- progress::progress_bar$new(
    total = length(l),
    format = "Finding maximum fontsize for element :name [:bar] :percent eta: :eta",
    show_after = show_after
  )
  # reduce has no handy way to name output, so we have to do this by hand
  if (test_named(l)) {
    list_names <- names(l)
  } else {
    list_names <- as.character(1:length(l))
  }
  pb$tick(0, tokens = list(name = list_names[1])) # start with 0 before first compute
  # ugly hack to get index right below
  allowed_fontsizes <- reduce(
    .x = l,
    .init = fontsizes_local_possible,
    .f = function(lhs, rhs, ...) {
      res <- find_fontsizes_1(fontsizes_local_possible = lhs, x = rhs,...)
      name <- list_names[min(c(match(x = list(rhs), table = l) + 1), length(l))]
      pb$tick(tokens = list(name = name))
      res
    },
    ... = ...
  )
  pb$terminate()
  allowed_fontsizes[length(allowed_fontsizes)]
}
find_fontsizes_1 <- function(fontsizes_local_possible = fontsizes_local, x, ...) {
  assert_subset(x = fontsizes_local_possible, choices = fontsizes_local)
  assert_character(x = fontsizes_local_possible, unique = TRUE, null.ok = FALSE)

  # always enforce proper order
  fontsizes_local_possible <- fontsizes_local_possible[order(
    match(
      x = fontsizes_local_possible, table = fontsizes_local
    )
  )]

  # calculate logical vector on *all* above allowed fontsizes
  # notice that this needs to run *all* fontsizes, because it's possible (given latex complexity) that, say fontsize 1 works, 2 fails and 3 works again
  # could happen because of other latex optimisations
  # so we're not saving runs here, because that might end up being only a local optimum
  requireNamespace2("fs")
  requireNamespace2("withr")
  working_fontsizes <- map_lgl(.x = fontsizes_local_possible, .f = function(this_size) {
    tex <- md2tex_mem(x = x, fontsize_local = this_size, ...)
    pdf <- texi2pdf2_mem(x = tex)
    out_path <- fs::path("test_fontsize", ext = "pdf")
    withr::local_file(file = out_path)
    readr::write_file(x = pdf, path = out_path, append = FALSE)
    test_pdf1page(x = out_path)
  })

  if (!(any(working_fontsizes))) {
    stop(
      glue(
        "Could not find a fontsize to fit the text on one page given the other arguments.",
        "Try shortening the text or using other additional arguments which take up less space.",
        .sep = " "
      ),
      call. = FALSE
    )
  }

  fontsizes_local_possible[working_fontsizes]
}
