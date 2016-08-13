#' @title Anonymize names.
#'
#' @export
#'
#' @description Replace real names with random names to anonymize data.
#'
#' @param real_names A character vector with unique names.
#'
#' @param lookup_file A character vector of length one as the file to write the lookup table to, or read the lookup table from.
#' Should be on a safe, private and encrypted volume.
#' If \code{lookup_file} does not exist, it is created.
#' See `notes` for caveats.
#'
#' @keywords internal
#'
#' @details
#' It is sometimes helpful in Q analyses to be able to refer to people-variables by a unique name, though real names often cannot be used in publications to protect participant's data.
#' This function replaces real names with random names, and also (as a side-effect) writes a lookup table with fake and real names to disk (as a \code{*.csv} file).
#' Uses names from \pkg{randomNames}.
#'
#' @note
#' \strong{
#'   Despite its name, this function \emph{does not magically anonymize data}, but merely replaces names with randomly drawn fake names.
#'   It is your responsibility to protect your participants' data.
#'   If you are unsure, or do not understand the below caveats, \emph{do not rely on this function}.
#'   \enumerate{
#'     \item The lookup table with real and fake names must be stored in a safe place, ideally \emph{encrypted} and \emph{no together with the raw data or results}.
#'     \item Your data may still be deanonymized if it includes other personal information and/or few participants.
#'     \item Make sure no `real names` are included in your command history, caches or other R objects and scripts.
#'   }
#' }
#'
#' @return
#' A character vector of fake names, same length as \code{real_names}.
#' Also writes a lookup table to disk at location \code{lookup_file}, if it does not exist already.
#'
#' @family helpers
#'
#' @examples
#' anonymize(real_names = c("Hillary", "Barack", "George"),
#'           lookup_file = system.file("extdata",
#'                                     "example_name_lookup.csv",
#'                                     package = "pensieve"))
#' # system.file call only necessary for example, shipped with pensieve
#' # just as an example, never store lookup file with raw data
#' # see `notes` for details
#'
#' @author Maximilian Held
#'

anonymize <- function(real_names, lookup_file) {
  # Input validation ====
  expect_character(x = real_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   null.ok = FALSE,
                   unique = TRUE)
  expect_string(x = lookup_file,
                na.ok = FALSE,
                null.ok = FALSE)
  expect_path_for_output(x = lookup_file,
                         overwrite = TRUE)

  # Body ====
  if (file.exists(lookup_file)) {  # then we just use it
    lookup <- utils::read.csv(file = lookup_file,
                              header = TRUE,
                              stringsAsFactors = FALSE)
    expect_equal(object = lookup$real_names,
                 expected = real_names)
    expect_character(x = lookup$fake_names,
                     any.missing = FALSE,
                     all.missing = FALSE,
                     unique = TRUE)
    for (i in lookup$fake_names) {
      expect_names(x = i, type = "strict", info = i)
    }
  } else {
    if (!requireNamespace("randomNames", quietly = TRUE)) {  # because this package is only in suggest
      stop("Package `randomNames` needed for this function to create random names. Please install it.",
           call. = FALSE)
    }

    all_names <- c(randomNames::randomNamesData$first_names_e1_g0,
                   randomNames::randomNamesData$first_names_e1_g1,
                   randomNames::randomNamesData$first_names_e2_g0,
                   randomNames::randomNamesData$first_names_e2_g1,
                   randomNames::randomNamesData$first_names_e3_g0,
                   randomNames::randomNamesData$first_names_e3_g1,
                   randomNames::randomNamesData$first_names_e4_g0,
                   randomNames::randomNamesData$first_names_e4_g1,
                   randomNames::randomNamesData$first_names_e5_g0,
                   randomNames::randomNamesData$first_names_e5_g1)
    # there seems to be no easy way to do this because the whole thing is only an environment
    unique_names <- unique(names(all_names))
    strict_names <- sapply(X = unique_names, FUN = function(x) {
      test_names(x = x, type = "strict")
    })
    good_names <- all_names[unique_names[strict_names]]

    lookup <- data.frame(
      real_names = real_names,
      fake_names = sample(x = names(good_names),
                          size = length(real_names),
                          replace = FALSE,
                          prob = good_names),
      stringsAsFactors = FALSE)
  }
  utils::write.csv(x = lookup, file = lookup_file, row.names = FALSE)
  return(lookup$fake_names)
}
