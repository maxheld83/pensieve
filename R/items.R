# items ====

#' @title Construct items
#'
#' @description Stores all item-related information in a list.
#'
#' @export
#'
#' @param pensieve_concourse object returned by [pensieveConcourse()]
pensieveItems <- function(pensieve_concourse) {
  pensieve_items <- new_pensieveItems(pensieve_concourse = pensieve_concourse)
  pensieve_items <- validate_pensieveItems(pensieve_items)
  return(pensieve_items)
}

# constructor
new_pensieveItems <- function(pensieve_concourse) {
  structure(
    .Data = list(
      concourse = pensieve_concourse
    ),
    class = c("pensieveItems")
  )
}

# validator
validate_pensieveItems <- function(pensieve_items) {
  validate_pensieveConcourse(pensieve_items$concourse)  # this also validates subclass, must not be null
  return(pensieve_items)
}


# concourse ====

#' @title Construct pensieveConcourse
#'
#' @description Stores *all* (researcher-facing) **item handles** and (participant-facing) **full items**.
#'
#' @export
#'
#' @param concourse
#' - for *monolingual studies*, when only one `languages` is given: a named character vector of (participant-facing) **full items**, with names as (researcher-facing) **item handles**,
#' - for *multilingual studies*, when several `languages` are given: a character matrix of (participant-facing) **full items**, with rownames as (researcher-facing) **item handles**.
#'
#' Full items must be unique by language, and can be `NA` if not available (not recommended).
#' Names must be unique and valid R names.
#'
#' @param languages a character vector of languages, in the order of columns in `concourse`.
#' Defaults to `c("english")`.
#' Must be unique and valid R names.
#'
#' @param type a character string giving the *kind* of item stimuli, must be one of:
#' - `"text"` for textual items, in which case cells in `concourse` must be text.
#'   An additional subclass `"pensieveConcourseText"` is prepended and validated.
#' - `"image"` for image items, in which case cells in `concourse` must be file paths, relative from `img_dir`.
#'   Images must be `*.png`, `*.jpg`, `*.jpeg` or `*.svg`.
#'   An additional class `"pensieveConcourseImage"` is prepended and validated.
#' Defaults to `"text"`.
#'
#' @param markup a character string giving the markup for `type = "text"`s.
#' Defaults to `"plain"` for plain text.
#' Currently only allows `"plain"`.
#'
#' @param img_dir a character string giving the directory for `type = "image"`s.
#' Must be relative path *from the working directory*.
#' Best constructed with [base::file.path()].
#' Defaults to `NULL`, in which case images are expected at the working directory root.
#'
#' @examples
#' # monolingual study, text items
#' monolingual_text <- pensieveConcourse(
#'   concourse = c(live_2_work = "Man lives to work.",
#'                 work_2_live = "Man works to live."),
#'   languages = c("english"),
#'   type = "text",
#'   markup = "plain"
#' )
#'
#' # multilingual study, text items
#' multilingual_text <- pensieveConcourse(
#'   concourse = matrix(
#'     data = c(
#'       "Man lives to work.", "Man lebt, um zu arbeiten.",
#'       "Man works to live.", "Man arbeitet, um zu leben."
#'     ),
#'     nrow = 2,
#'     ncol = 2,
#'     dimnames = list(items = c("live_2_work", "work_2_live"))
#'   ),
#'   languages = c("english", "ngerman"),
#'   type = "text",
#'   markup = "plain"
#' )
#'
#' # monolingual study, image items
#' monolingual_image <- pensieveConcourse(
#'  concourse = c(peach = "peach.jpg", pear = "pear.jpg"),
#'  img_dir = file.path(system.file(package = "pensieve"), "extdata", "fruit"),
#'  # these files ship with pensieve
#'  type = "image"
#' )

#'
#' @family constructors
pensieveConcourse <- function(concourse, languages = c("english"), type = "text", markup = "plain", img_dir = NULL) {
  assert_string(x = type, na.ok = FALSE, null.ok = FALSE)
  assert_character(x = languages,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   min.len = 1,
                   unique = TRUE,
                   null.ok = FALSE)

  # monolingual case, easier entry ui
  if (length(languages) == 1 & is.vector(concourse)) {
    assert_vector(x = concourse,
                  strict = TRUE,
                  any.missing = TRUE,
                  all.missing = TRUE,
                  unique = TRUE,
                  names = "strict")
    concourse <- as.matrix(concourse)
    colnames(concourse) <- languages
  }

  # general and multilingual case
  if (length(languages) != ncol(concourse)) {
    stop(
      paste("You provided", length(languages), "languages, but concourse has", ncol(concourse), "columns.",
            "Must be equal."),
      call. = FALSE
    )
  }

  # construction
  if (type == "text") {
    concourse <- new_pensieveConcourseText(concourse = concourse,
                                           handles = rownames(concourse),
                                           languages = languages,
                                           markup = markup)
  } else if (type == "image") {
    concourse <- new_pensieveConcourseImage(concourse = concourse,
                                            handles = rownames(concourse),
                                            languages = languages,
                                            img_dir = img_dir)
  }

  # validation
  validate_pensieveConcourse(concourse = concourse)

  return(concourse)
}

# parent constructor
new_pensieveConcourse <- function(concourse, handles, languages, ..., subclass) {
  structure(
    .Data = concourse,
    dimnames = list(handles = handles, languages = languages),
    ...,
    class = c(subclass, "pensieveConcourse", "matrix")
  )
}

# parent validator
validate_pensieveConcourse <- function(concourse) {
  assert_matrix(x = concourse,
                mode = "character",
                any.missing = TRUE,
                all.missing = TRUE,
                row.names = "strict",
                col.names = "strict",
                null.ok = FALSE)

  assert_unique_in_column(x = concourse)

  # validate subclasses
  if (inherits(x = concourse, what = "pensieveConcourseText")) {
    validate_pensieveConcourseText(concourse = concourse)
  } else if (inherits(x = concourse, what = "pensieveConcourseImage")) {
    validate_pensieveConcourseImage(concourse = concourse)
  } else {
    stop("No valid type provided. Must be 'text' or 'image'.",
         call. = FALSE)
  }

  return(concourse)
}

# subclass text
new_pensieveConcourseText <- function(concourse, handles, languages, markup) {
  concourse <- new_pensieveConcourse(concourse = concourse,
                                     handles = handles,
                                     markup = markup,
                                     languages = languages,
                                     subclass = "pensieveConcourseText")
  return(concourse)
}

validate_pensieveConcourseText <- function(concourse) {
  markup <- attr(x = concourse, which = "markup")
  assert_string(x = markup, na.ok = FALSE, null.ok = FALSE)
  assert_choice(x = markup, choices = c("plain"), null.ok = FALSE)
  #TODO test whether strings *are* in this markup, and valid
  return(concourse)
}

# subclass image
new_pensieveConcourseImage <- function(concourse, handles, languages, img_dir) {
  concourse <- new_pensieveConcourse(concourse = concourse,
                                     handles = handles,
                                     img_dir = img_dir,
                                     languages = languages,
                                     subclass = "pensieveConcourseImage")
  return(concourse)
}

validate_pensieveConcourseImage <- function(concourse) {
  img_dir <- attr(x = concourse, which = "img_dir")
  assert_string(x = img_dir, na.ok = FALSE, null.ok = TRUE)
  if (!is.null(img_dir)) {
    assert_directory_exists(x = img_dir, access = "r")
    files <- file.path(img_dir, as.vector(concourse))
  } else {
    files <- file.path(as.vector(concourse))
  }
  assert_file_exists(x = files,
                     extension = c("png", "jpg", "jpeg", "svg"),
                     access = "r",
                     .var.name = "file names")
  return(concourse)
}

#' @title Custom print method for knitr
#'
#' @description Provides custom print method for knitr.
#' Can also be invoked manually to open interactive outputs in RStudio.
#'
#' @param x object returned by [pensieveConcourse()].
#'
#' @template plot
#'
#' @inheritParams knitr::knit_print
#'
#' @export
#'
#' @family knitr output functions
knit_print.pensieveConcourse <- function(x, use_js = NULL, ...) {
  # Input validation ====
  use_js <- assert_n_infer_use_js(use_js = use_js)

  validate_pensieveConcourse(x)

  # JS method ====
  if (use_js) {  # interactive
    DT::datatable(data = as.data.frame(x),
                  options = list(searchHighlight = TRUE))
  } else {
    print(x)
  }
}



# QSet ====

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
                       units = "cm") {

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
  tex_file <- file.path(output_dir, paste0(item_handle, ".tex"))
  assets <- NULL
  assets$paths <- NULL
  assets$bin <- NULL

  # render
  knitr::knit(input = input_path, output = tex_file)
  assets$paths$latex <- tex_file
  withr::with_dir(new = output_dir, code = {
    # changing wd with with_dir is sadly necessary because below utilities do not offer convenient output paths
    tools::texi2pdf(file = tex_file)
    assets$paths$pdf <- file.path(output_dir, paste0(item_handle, ".pdf"))
    assets$bin$pdf <- readr::read_file_raw(file = assets$paths$pdf)
    pdf2svg(pdf_input = paste0(item_handle, ".pdf"))
    assets$paths$svg <- file.path(output_dir, paste0(item_handle, ".svg"))
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


# add arbitrary babel invocation
latex$options$babel <- c(
  # this list is from https://tug.org/pracjourn/2007-1/gregorio/gregorio.pdf
  "acadian",
  "afrikaans",
  "albanian",
  "american",
  "australian",
  "austrian",
  "bahasa",
  "bahasai",
  "bahasam",
  "basque",
  "brazil",
  "brazilian",
  "breton",
  "british",
  "bulgarian",
  "canadian",
  "canadien",
  "catalan",
  "croatian",
  "czech",
  "danish",
  "dutch",
  "english",
  "esperanto",
  "estonian",
  "finnish",
  "francais",
  "french",
  "frenchb",
  "galician",
  "german",
  "germanb",
  "greek",
  "hebrew",
  "hungarian",
  "icelandic",
  "indon",
  "indonesian",
  "interlingua",
  "irish",
  "italian",
  "latin",
  "lowersorbian",
  "magyar",
  "ngerman",
  "norsk",
  "nynorsk",
  "polish",
  "polutonikogreek",
  "portuges",
  "portuguese",
  "romanian",
  "russian",
  "samin",
  "scottish",
  "serbian",
  "slovak",
  "slovene",
  "spanish",
  "swedish",
  "turkish",
  "ukrainian",
  "uppersorbian",
  "welsh",
  "UKenglish",
  "USenglish",
  "malay",
  "meyalu",
  "naustrian",
  "newzealand"
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


# QItemSample ====
# logical vector about the inclusion/exclusion of items
# make this a function which *actually* samples

# QItemStrata ====
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


# QItemFeatures ====
# WIDE dataframe with arbitrary features of the items, one row per item


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
