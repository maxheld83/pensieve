imports <- devtools::parse_ns_file()$imports  # capture all imports from namespace file
imports <- purrr::keep(.x = imports, .p = purrr::negate(is.list))  # only take the full imports
purrr::walk(.x = imports, .f = library, character.only = TRUE)

devtools::load_all()
