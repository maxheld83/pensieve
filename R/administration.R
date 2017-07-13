# making cards ====

make_cards <- function(text,
                       fontsize = "normalsize",
                       language = NULL,
                       paperwidth = 16,
                       paperheight = 9,
                       top = 1,
                       bottom = 1,
                       left = 1,
                       right = 1,
                       units = "cm",
                       template = "simple_rect.Rnw") {

  # Input validation
  checkmate::assert_string(x = text, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  checkmate::assert_choice(x = template, choices = c("simple_rect.Rnw"), null.ok = FALSE)
  # other inputs are validated in downstream functions

  # render
  input_path <- paste0(
    file.path(system.file(package = "pensieve")),
    "/templates/cards/",
    template
  )
  knitr::knit2pdf(input = input_path, quiet = FALSE)
}


# helper to convert pdf to svg
pdf2svg <- function(pdf_input) {
  # Input validation
  checkmate::assert_file_exists(x = pdf_input, extension = "pdf")
  checkmate::assert_character(x = pdf_input, any.missing = FALSE, unique = TRUE)
  checkmate::assert_path_for_output(x = getwd(), overwrite = TRUE)
  checkmate::assert_os(os = c("mac", "linux"))

  # vectorized!
  for (i in pdf_input) {
    file_i <- tools::file_path_sans_ext(i)
    # assemble command
    system2(command = "pdf2svg",
            args = c(i, paste0(file_i, ".svg"), "1"),
            stderr = "")
  }
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
