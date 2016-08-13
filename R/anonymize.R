#' @title Anonymize names.
#'
#' @export
#'
#' @description Replace real names with random names to anonymize data.
#'
#' @param real_names A character vector with unique names.
#'
#' @param lookup_file A character vector of length one as the file to write the lookup table to, or read the lookup table from.
#' See `details` for more, `notes` for caveats.
#'
#' @keywords internal
#'
#' @details
#' It is sometimes helpful in Q analyses to be able to refer to people-variables by a unique name, though real names often cannot be used in publications to protect participant's data.
#'
#' This function looks up \code{real_names} in the \code{lookup_file} and returns the respective fake names.
#'
#' The \code{lookup_file} must always be a \code{*.csv}-file with two columns of character vectors, named `real_names` and `fake_names`.
#'
#' If the specified \code{lookup_file} does not exist, new random `fake_names` are sampled from \pkg{randomNames}, and written to disc as the specified file.
#' Generated `fake_names` are unique and can be used as R variable names.
#'
#' If the \code{lookup_file} does not include all \code{real_names}, it is likewise appended with new `fake_names`.
#' All entries must be unique and valid R variable names.
#' The rows in the \code{lookup_file} can be in an arbitrary order, and can also include entries that are never used.
#'
#' By providing a \code{lookup_file} users (or participants) can choose their own `fake_names`, though this may not protect personal data well.
#' In particular, storing socio-demographic data of participants as custom `fake_names` (such as, for example \code{"m_us_31"}) is not advised, because such data may be easily breached and downstream functions expect socio-demographic data in a different format.
#'
#' @note
#' \strong{
#'   Despite its name, this function \emph{does not magically anonymize data}, but merely replaces names with randomly drawn fake names.
#'   It is your responsibility to protect your participants' data.
#'   If you are unsure, or do not understand the below caveats, \emph{do not rely on this function}.
#'   \enumerate{
#'     \item The lookup table with real and fake names must be stored in a safe place, ideally \emph{encrypted} and \emph{not together with the raw data or results}.
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

  # let's first build the resulting df with empty fake_names
  lookup <- data.frame(real_names = real_names,
                       fake_names = NA,
                       stringsAsFactors = FALSE)

  if (file.exists(lookup_file)) {  # then we fill in what we can
    file <- utils::read.csv(file = lookup_file,
                            header = TRUE,
                            stringsAsFactors = FALSE)
    # df ok?
    expect_data_frame(x = file,
                      types = c("character", "character"),
                      any.missing = FALSE,
                      all.missing = FALSE,
                      ncols = 2,
                      null.ok = FALSE)
    expect_equal(object = names(file),
                 expected = c("real_names", "fake_names"))

    # let's check those that we have, are the fake names ok?
    expect_character(x = file$fake_names,
                     any.missing = FALSE,
                     all.missing = FALSE,
                     unique = TRUE)
    for (i in file$fake_names) {
      expect_names(x = i, type = "strict", info = i)  # are they valid r varnames?
    }

    # let's also protect against non-unique real_names
    expect_character(x = file$real_names,
                     any.missing = FALSE,
                     all.missing = FALSE)

    # now let's fill in what we have
    # must be via for-loop to make sure order doesn't matter
    for (i in lookup$real_names) {
      if (length(file[file$real_names == i, "fake_names"]) < 1) {
        lookup$fake_names[lookup$real_names == i] <- NA
      } else {
        lookup$fake_names[lookup$real_names == i] <- file[file$real_names == i, "fake_names"]
      }
    }
  }

  needed_names <- sum(is.na(lookup$fake_names))
  if (needed_names > 0) {
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
    unused_names <- all_names[!names(all_names) %in% lookup$fake_names]
    unique_names <- unique(names(unused_names))
    strict_names <- sapply(X = unique_names, FUN = function(x) {
      test_names(x = x, type = "strict")
    })
    good_names <- all_names[unique_names[strict_names]]

    lookup$fake_names[is.na(lookup$fake_names)] <- sample(x = names(good_names),
                                                          size = needed_names,
                                                          replace = FALSE,
                                                          prob = good_names)
    utils::write.table(x = lookup,
                       sep = ",",
                       file = lookup_file,
                       row.names = FALSE,
                       append = FALSE)
  }
  return(lookup$fake_names)
}
