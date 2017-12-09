# items ====

#' @title Construct list of item-related objects
#'
#' @export
#'
#' @param ps_concourse object returned by [psConcourse()]
#'
#' @template construction_helpers
psItems <- function(ps_concourse) {
  validate_psItems(new_psItems(ps_concourse = ps_concourse))
}

# constructor
new_psItems <- function(ps_concourse) {
  structure(
    .Data = list(
      concourse = ps_concourse
    ),
    class = c("psItems")
  )
}

# validator
validate_psItems <- function(ps_items) {
  validate_psConcourse(ps_items$concourse)  # this also validates subclass, must not be null
  return(ps_items)
}


# psConcourse ====

#' @title Store *all* **full items** in a psConcourse matrix.
#'
#' @export
#'
#' @param concourse
#' A matrix with:
#' - researcher-facing **item handles** as row names,
#' - **languages** as column names and
#' - participant-facing **full items** in cells.
#'
#' For *monolingual studies*, use one column only.
#'
#' Full items must be unique by language, and can be `NA` if not available (not recommended).
#' Names must be unique and valid R names.
#'
#' [as_psConcourse()] also accepts `concourse` as:
#' - `data.frame()`,
#' - `matrix()`,
#' - named character vector (`c()`).
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
#' @examples
#' # multilingual study, text items
#' multilingual_text <- psConcourse(
#'   concourse = matrix(
#'     data = c(
#'       "Man lives to work.", "Man lebt, um zu arbeiten.",
#'       "Man works to live.", "Man arbeitet, um zu leben."
#'     ),
#'     nrow = 2,
#'     ncol = 2,
#'     dimnames = list(
#'       items = c("live_2_work", "work_2_live"),
#'       languages = c("english", "ngerman"))
#'   ),
#'   type = "text",
#'   markup = "plain",
#'   babel = TRUE
#' )
#'
#' # monolingual study, image items
#' monolingual_image <- psConcourse(
#'   concourse = matrix(
#'     data = c("peach.jpg",
#'              "pear.jpg"),
#'     nrow = 2,
#'     ncol = 1,
#'     dimnames = list(
#'       items = c("peach", "pear"),
#'       languages = c("english")
#'    )),
#'  type = "image",
#'  img_dir = file.path(system.file(package = "pensieve"), "extdata", "fruit")
#'  # these files ship with pensieve
#' )
#'
#' @template construction_helpers
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
    class = c(subclass, "psConcourse", "matrix"))
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


# subclass text ===

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


# subclass image

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


# coercion

#' @rdname psConcourse
#' @export
as_psConcourse <- function(concourse,
                           type = "text",
                           markup = "plain",
                           babel = TRUE,
                           img_dir = NULL,
                           ...) {
  UseMethod(generic = "as_psConcourse")
}

#' @export
as_psConcourse.default <- function(concourse, type, markup, babel, img_dir, ...) {
  stop(
    "Sorry, don't know how to coerce object of class ",
    paste(class(concourse), collapse = "/"),
    " into a psConcourse.",
    call. = FALSE
  )
}

#' @export
as_psConcourse.psConcourse <- function(concourse, ...) {
  psConcourse(concourse)
}

#' @describeIn psConcourse coerce matrices to psConcourse
#'
#' @param languages character vector as an alternative way to give the languages of items.
#' Defaults to `NULL`, in which case languages are expected as column names.
#'
#' @param handles character vector as an alternative way to give the item handles.
#' Defaults to `NULL`, in which case items are expected as row names.
#'
#' @examples
#' # coerce matrix to psConcourse (multilingual concourse)
#' concourse <- matrix(
#'   data = c(
#'     "Man lives to work.",
#'     "Man lebt, um zu arbeiten.",
#'     "Man works to live.",
#'     "Man arbeitet, um zu leben."),
#'   nrow = 2,
#'   ncol = 2)
#' concourse <- as_psConcourse(
#'   concourse = concourse,
#'   languages = c("english", "ngerman"),
#'   handles = c("live_2_work", "work_2_live"))
#'
#' @export
as_psConcourse.matrix <- function(concourse,
                                  type = "text",
                                  markup = "plain",
                                  babel = TRUE,
                                  img_dir = NULL,
                                  languages = NULL,
                                  handles = NULL,
                                  ...) {
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
#' @examples
#' # coerce data.frame to psConcourse (multilingual concourse)
#' concourse <- data.frame(
#'   english = c("man lives to work", "man works to live"),
#'   ngerman = c("man lebt, um zu arbeiten", "man arbeitet, um zu leben"))
#' as_psConcourse(concourse = concourse, handles = c("live_2_work", "work_2_live"))
#'
#' @export
as_psConcourse.data.frame <- as_psConcourse.matrix


#' @describeIn psConcourse coerce named character vector to psConcourse (monolingual concourse)
#'
#' @examples
#' # coerce character vector to psConcourse (monolingual concourse only)
#' concourse <- c(
#'   live_2_work = "man lives to work",
#'   work_2_live = "man works to live")
#' as_psConcourse(concourse, languages = "english")
#'
#' @export
as_psConcourse.character <- as_psConcourse.matrix


#' @describeIn psConcourse print psConcourse in knitr chunks
#'
#' @template print
#'
#' @examples
#' # print concourse
#' knitr::knit_print(x = multilingual_text, use_js = TRUE, options = NULL)
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

make_cards <- function(item_text,
                       item_handle,
                       template = "simple_rect.Rnw",
                       output_dir = tempdir(),
                       fontsize = "normalsize",
                       language = NULL,
                       paperwidth = 16,
                       paperheight = 9,
                       top = 1,
                       bottom = 1,
                       left = 1,
                       right = 1,
                       units = "cm",
                       extra_preamb_args = NULL,
                       alignment = "justified") {

  # Input validation
  checkmate::assert_string(x = item_text, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  checkmate::assert_string(x = item_handle, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  checkmate::assert_choice(x = template, choices = c("simple_rect.Rnw"), null.ok = FALSE)
  checkmate::assert_directory(x = output_dir, access = "w")
  # other inputs are validated in downstream functions

  # find template
  in_build_path <- file.path("templates", "cards", template)
  in_source_path <- file.path("inst", in_build_path)
  # this is just to make debugging easier; if run from pensieve source dir, then use source template
  if (file.exists(in_source_path)) {
    input_path <- in_source_path
  } else {
    input_path <- file.path(system.file(package = "pensieve"), in_build_path)
  }

  # other prep
  assets <- NULL
  assets$paths <- NULL
  assets$bin <- NULL
  assets$paths$latex <- file.path(output_dir, paste0(item_handle, ".tex"))
  assets$paths$pdf <- file.path(output_dir, paste0(item_handle, ".pdf"))
  assets$paths$svg <- file.path(output_dir, paste0(item_handle, ".svg"))

  # render
  knitr::knit(input = input_path, output = assets$paths$latex)
  assets$bin$latex <- readr::read_file(assets$paths$latex)
  withr::with_dir(new = output_dir, code = {
    # changing wd with with_dir is sadly necessary because below utilities do not offer convenient output paths
    # setwd(output_dir)
    tools::texi2pdf(file = assets$paths$latex)
    assets$bin$pdf <- readr::read_file_raw(file = assets$paths$pdf)
    pdf2svg(pdf_input = paste0(item_handle, ".pdf"))
    assets$bin$svg <- readr::read_file(file = assets$paths$svg)
    # setwd("/Users/max/GitHub/pensieve/")
  })
  return(assets)
}

# helper to convert pdf to svg
pdf2svg <- function(pdf_input) {
  # Input validation
  checkmate::assert_file_exists(x = pdf_input, extension = "pdf")
  checkmate::assert_character(x = pdf_input, any.missing = FALSE, unique = TRUE)
  checkmate::assert_os(os = c("mac", "linux"))

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


# helpers to create latex formatting instructions
latex <- list(set = NULL, # these are the functions
              options = NULL) # these are the available options

# insert arbitrary fontsize

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

latex$set$fontsize <- function(fontsize) {
  # fontsize <- "tiny"
  checkmate::assert_character(x = fontsize, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = fontsize, choices = latex$options$fontsize, null.ok = FALSE)

  cat("\\", fontsize, "\n", sep = "")
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

latex$set$alignment <- function(alignment = "justified") {
  # alignment <- "justified"
  checkmate::assert_character(x = alignment, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = alignment, choices = latex$options$alignment, null.ok = FALSE)

  switch(
    EXPR = alignment,
    left = cat("\\raggedright \n "),
    right = cat("\\raggedleft \n "),
    center = cat("\\centering \n ")
  )
}

# add arbitrary commands to preamble
latex$set$extra_preamb_args <- function(extra_preamb_args) {
  # type faces can be found here: http://www.tug.dk/FontCatalogue/universalisadfstandard/

  checkmate::assert_character(x = extra_preamb_args, any.missing = FALSE)

  cat(extra_preamb_args, sep = "\n")
}

# add arbitrary babel invocation
latex$options$babel <- c(
  # this list is from http://ctan.math.washington.edu/tex-archive/macros/latex/required/babel/base/babel.pdf
  "afrikaans",
  "azerbaijani",
  "basque",
  "breton",
  "bulgarian",
  "catalan",
  "croatian",
  "czech",
  "danish",
  "dutch",

  # english
  "english",
  "USenglish",
  "american",
  "UKenglish",
  "british",
  "canadian",
  "australian",
  "newzealand",

  "esperanto",
  "estonian",
  "finnish",

  # french
  "french",
  "francais",
  "canadien",
  "acadian",

  "galician",

  # german
  "austrian",
  "german",
  "germanb",
  "ngerman",
  "naustrian",

  # greek
  "greek",
  "polutonikogreek",

  "hebrew",
  "icelandic",

  # indonesian
  "bahasa",
  "indonesian",
  "indon",
  "bahasai",

  "interlingua",
  "irish", # gaelic
  "italian",
  "latin",
  "lowersorbian",

  # malay
  "bahasam",
  "malay",
  "melayu",

  "samin",

  # norwegian
  "norks",
  "nynorsk",

  "polish",

  # portuguese
  "portuges",
  "portuguese",
  "brazilian",
  "brazil",

  "romanian",
  "russian",
  "scottish",
  "spanish",
  "slovak",
  "slovene",
  "swedish",
  "serbian",
  "turkish",
  "urkainian",
  "uppersorbian",
  "welsh"
)

latex$set$babel <- function(language) {
  checkmate::assert_character(x = language, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = language, choices = latex$options$babel, null.ok = FALSE)

  cat(
    "\\usepackage[",
    language,
    "]{babel}",
    "\n",
    sep = ""
  )
}


# set arbitrary dimensions
latex$set$geometry <- function(paperwidth, paperheight, top, bottom, left, right, units) {
  checkmate::assert_character(x = units, any.missing = FALSE, len = 1)
  checkmate::assert_choice(x = units, choices = c("cm", "in"), null.ok = FALSE)

  all_args <- as.list(environment())
  num_args <- all_args[!(names(all_args) == "units")]

  mapply(FUN = checkmate::assert_numeric,
         x = num_args,
         .var.name = names(num_args),
         MoreArgs = list(
           lower = 0,
           finite = TRUE,
           any.missing = FALSE,
           len = 1,
           null.ok = FALSE
         )
  )

  opts <- sapply(
    X = names(num_args),
    FUN = function(x) {
      return(paste0(
        x,
        "=",
        num_args[[x]],
        units
      ))
    }
  )
  opts <- c(opts, "vcentering", "hcentering")  # add vertical, horizontal center just to be safe

  cat(
    "\\usepackage[",
    paste(opts, collapse = ", "),
    "]{geometry}",
    "\n",
    sep = ""
  )
}

# pensieveItemStrata ====
# logical array of n dimensions, items as rows, arbitrary dimensions,
#' @title Check and make QItemStrata
#'
#' @export
#'
#' @description Checks and makes QItemStrata, the item sampling structure
#'
#' @param strata A logical array of arbitrary dimensions, with first dimension (rows) as item handles, and higher dimensions as orthogonal sampling strata.
#'
#' @template construct
#'
#' @family import helpers
QItemStrata <- produce_class_constructor(classname = "QItemStrata", fun = function(strata) {
  return(strata)
})

#' @export
#' @rdname check
check.QItemStrata <- function(x) {
  res <- NULL
  res$array <- check_array(x = x,
                           mode = "logical",
                           any.missing = FALSE,
                           min.d = 1,
                           null.ok = FALSE)
  res <- c(res, check_named_array(x = x))  # via external helper
  return(report_checks(res = res, info = "QItemStrata"))
}


# Drafts and Helpers ====
# helper: check QLookup
check_QLookup <- check_lookup <- function(x){
  res <- NULL
  res$tibble <- check_tibble(x = x,
                             types = c("integerish", "integer", "character"),
                             any.missing = TRUE,  # some NAs are permissible
                             all.missing = FALSE,
                             null.ok = FALSE)

  res$nna_row <- check_nna_row(x = x[,-1])  # we do not care about first column

  # check whether the lookup table is all unique by columns, as is must be
  # notice the being non-unique by row does not matter
  res$unique_column <- check_unique_in_column(x = x)

  return(report_checks(res = res, info = "lookup"))
}


# helper: check Q_set vs lookup
check_qset_v_lookup <- function(q_set, lookup) {
  if (all(q_set %in% lookup$item_handle)) {
    return(TRUE)
  } else {
    return(paste("All item handles in",
                 vname(q_set),
                 "must be in",
                 vname(lookup)))
  }
}


# TODO this would be a helpful hint for people who only have bad q_set
# #' @importFrom lettercase make_names
# #' @name make_names
# #' @rdname make_names
# #' @export
# NULL
