# QItems ====
# this ties it all together in a list, checks for consistency

# QSet ====

# QItem ====

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
QItemStrata <- function(strata, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  strata <- classify_clever(x = strata, classname = "QItemStrata")

  assert_class2(x = strata, validate = validate)

  return(strata)
}

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

# QConcourse ====
#' @title Check and make QConcourse
#'
#' @description Check and make QConcourse
#'
#' @export
#'
#' @param concourse A character matrix of full items, with named rows as item handles and named columns as languages.
#' Cells can be `NA` when full items are not available.
#' Full items must be unique by columns.
#'
#' @template construct
#'
#' @family import helpers
#'
QConcourse <- function(concourse, validate = TRUE) {
  assert_flag(x = validate,
              na.ok = FALSE,
              null.ok = FALSE)

  concourse <- classify_clever(x = concourse, classname = "QConcourse")

  assert_class2(x = concourse, validate = validate)

  return(concourse)
}

#' @export
#' @rdname check
check.QConcourse <- function(x) {
  res <- NULL

  res$matrix <- check_matrix(x = x,
                             mode = "character",
                             any.missing = TRUE,
                             all.missing = TRUE,
                             row.names = "strict",
                             col.names = "strict",
                             null.ok = FALSE)

  # check whether the concourse is all unique by columns, as is must be:
  # makes no sense to have same item twice
  # notice that being non-unique by row does not matter:
  # conceivable, though unlikely, that items are same in two languages
  res$unique_by_column <- check_unique_in_column(x = x)

  return(report_checks(res = res, info = "QConcourse"))
}

#' @title Custom print method for knitr
#'
#' @description Provides custom print method for knitr.
#' Can also be invoked manually to open interactive outputs in RStudio.
#'
#' @param x a character matrix with full item wording of class [`QConcourse`][QConcourse], as created by [QConcourse()].
#'
#' @template plot
#'
#' @inheritParams knitr::knit_print
#'
#' @export
#'
#' @family knitr output functions
knit_print.QConcourse <- function(x, use_js = NULL, ...) {
  # Input validation ====
  use_js <- assert_n_infer_use_js(use_js = use_js)

  x <- classify_clever(x = x, classname = "QConcourse")  # gotta make sure it IS QItems in the first place
  assert(x)

  # JS method ====
  if (use_js) {  # interactive
  DT::datatable(data = as.data.frame(x),
                options = list(searchHighlight = TRUE))
  } else {
    print(x)
  }
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
