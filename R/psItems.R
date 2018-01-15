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
  English = "english",
  `English (US)` = "USenglish",
  `English (America)` = "american",
  `English (UK)` = "UKenglish",
  `English (Britain)` = "british",
  `English (Canada)` = "canadian",
  `English (Australia)` = "australian",
  `English (New Zealand)` = "newzealand",

  Afrikaans = "afrikaans",
  Azerbaijani = "azerbaijani",
  Basque = "basque",
  Breton = "breton",
  Bulgarian = "bulgarian",
  Catalan = "catalan",
  Croatian = "croatian",
  Czech = "czech",
  Danish = "danish",
  Dutch = "dutch",

  Esperanto = "esperanto",
  Estonian = "estonian",
  Finnish = "finnish",

  # french
  French = "french",
  `French (Canada)` = "canadien",
  `French (Acadian)` = "acadian",

  Galician = "galician",

  # german
  `German (Austria)` = "austrian",
  German = "german",
  `German (DACH)` = "germanb",
  `German (Germany New)` = "ngerman",
  `German (Austria New)` = "naustrian",

  # greek
  Greek = "greek",
  `Greek (Polutonik Accents)` = "polutonikogreek",

  Hebrew = "hebrew",
  Icelandic = "icelandic",

  # indonesian
  `Indonesian` = "indonesian",
  `Indonesian (Bahasa)` = "bahasa",
  `Indonesian (Indon)` = "indon",
  `Indonesian (Bahasai)` = "bahasai",

  Interlingua = "interlingua",
  `Irish (Gaelic)` = "irish",
  Italian = "italian",
  Latin = "latin",
  `Lower Sorbian` = "lowersorbian",

  # malay
  `Malay (Bahasam)` = "bahasam",
  Malay = "malay",
  `Malay (Melayu)` = "melayu",

  Samin = "samin",

  # norwegian
  `Norwegian (Norks)` = "norks",
  `Norwegian (Nynorks)` = "nynorsk",

  Polish = "polish",

  # portuguese
  # "portuges",
  Portuguese = "portuguese",
  `Portuguese (Brazil)` = "brazilian",
  # "brazil",

  Romanian = "romanian",
  Russian = "russian",
  Scottish = "scottish",
  Spanish = "spanish",
  Slovak = "slovak",
  Slovene = "slovene",
  Swedish = "swedish",
  Serbian = "serbian",
  Turkish = "turkish",
  Ukrainian = "ukrainian",
  `Upper Sorbian` = "uppersorbian",
  Welsh = "welsh"
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
