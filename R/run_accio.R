# find path to *built* accio, always root of pensieve
# this gives "" if folder does not exist
accio_path <- system.file('accio', package = 'pensieve')

#' @title Run accio
#'
#' @description
#' Run accio, the web frontend for pensieve, if available.
#'
#' @details
#' Accio is the closed-source web frontend for pensieve.
#' Learn more at https://pensieve.maxheld.de.
#'
#' @export
run_accio <- function() {
  # input validation ====
  if (!checkmate::test_directory_exists(accio_path)) {
    stop("The 'accio' web frontend (closed source) is not available on this computer. ",
         "See pensieve.maxheld.de for details.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # run it! ====
  shiny::runApp(appDir = accio_path)
}
