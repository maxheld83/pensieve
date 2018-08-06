if (Sys.getenv()["LOGNAME"] %in% c("max")) {
  message("I'm running some preparatory steps because this is a dev machine.")
  imports <- devtools::parse_ns_file()$imports  # capture all imports from namespace file
  imports <- purrr::discard(.x = imports, .p = is.list)  # only take the full imports
  purrr::walk(.x = imports, .f = library, character.only = TRUE)

  devtools::load_all()
}


