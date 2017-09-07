#' @title Anonymize names.
#'
#' @export
#'
#' @description Create random names and/or replace real names with random names to anonymize data.
#'
#' @param real_names A vector with unique names.
#' If there are no known real names, provide vector of unique integers instead.
#'
#' @param lookup_file A character vector of length one as the file to write the lookup table to, or read the lookup table from.
#' See **details** for more, **notes** for caveats.
#' File should not be published to preserve the identity of participants.
#'
#' @keywords internal
#'
#' @family import helpers
#'
#' @details
#' It is sometimes helpful in Q analyses to be able to refer to people-variables by a unique name, though real names often cannot be used in publications to protect participant's data.
#'
#' This function looks up `real_names` in the `lookup_file` and returns the respective fake names.
#'
#' The `lookup_file` must always be a `*.csv`-file with two columns of character vectors, named `real_names` and `fake_names`.
#'
#' If the specified `lookup_file` does not exist, new random `fake_names` are sampled from `randomNames`, and written to disc as the specified file.
#' Generated `fake_names` are unique and can be used as R variable names.
#'
#' If the `lookup_file` does not include all `real_names`, it is likewise appended with new `fake_names`.
#' All entries must be unique and valid R variable names.
#' The rows in the `lookup_file` can be in an arbitrary order, and can also include entries that are never used.
#'
#' By providing a `lookup_file` users (or participants) can choose their own `fake_names`, though this may not protect personal data well.
#' In particular, storing socio-demographic data of participants as custom `fake_names` (such as, for example `"m_us_31"`) is not advised, because such data may be easily breached and downstream functions expect socio-demographic data in a different format.
#'
#' @note
#' **Despite its name, this function *does not magically anonymize data*, but merely replaces names with randomly drawn fake names.
#' It is your responsibility to protect your participants' data.
#' If you are unsure, or do not understand the below caveats, *do not rely on this function*.**
#'
#' 1. The lookup table with real and fake names must be stored in a safe place, ideally *encrypted* and *not together with the raw data or results*.
#' 2. Your data may still be deanonymized if it includes other personal information and/or few participants.
#' 3. Make sure no `real names` are included in your command history, caches or other R objects and scripts.
#'
#' @return
#' A character vector of fake names, same length as `real_names`.
#' Also writes a lookup table to disk at location `lookup_file`, if it does not exist already.
#'
#' @family import helpers
#'
#' @examples
#' anonymize(real_names = c("Hillary", "Barack", "George"),
#'           lookup_file = system.file("extdata",
#'                                     "example_name_lookup.csv",
#'                                     package = "pensieve"))
#' # system.file call only necessary for example, shipped with pensieve
#' # just as an example, never store lookup file with raw data
#' # see `notes` for details

anonymize <- function(real_names, lookup_file) {
  # Input validation ====
  expect_vector(x = real_names,
                any.missing = FALSE,
                all.missing = FALSE,
                null.ok = TRUE,
                unique = TRUE)
  expect_string(x = lookup_file,
                na.ok = FALSE,
                null.ok = FALSE)
  expect_path_for_output(x = lookup_file,
                         overwrite = TRUE)

  # Body ====

  if (file.exists(lookup_file)) {  # then we fill in what we can
    lookup <- utils::read.csv(file = lookup_file,
                              header = TRUE,
                              stringsAsFactors = FALSE,
                              colClasses = c("character", "character"))

    info <- "This problem stems from your 'lookup_file'."
    expect_data_frame(x = lookup,
                      types = c("character", "character"),
                      any.missing = FALSE,
                      all.missing = FALSE,
                      ncols = 2,
                      null.ok = FALSE,
                      info = info)
    expect_equal(object = names(lookup),
                 expected = c("real_names", "fake_names"),
                 info = info)

    # let's check those that we have, are the fake names ok?
    expect_character(x = lookup$fake_names,
                     any.missing = FALSE,
                     all.missing = FALSE,
                     unique = TRUE,
                     info = info)
    expect_names(x = lookup$fake_names,
                 type = "strict",
                 info = info)

    expect_character(x = lookup$real_names,
                     unique = TRUE,
                     info = info)

    expect_character(x = c(lookup$fake_names, lookup$real_names),
                     unique = TRUE,
                     any.missing = FALSE,
                     all.missing = FALSE,
                     info = "Your 'lookup_info' must not contain missing or non-unique values across and within both columns.")

  } else {
    lookup <- data.frame(real_names = real_names,
                         fake_names = NA,
                         stringsAsFactors = FALSE)
  }

  for (i in real_names) {
    if (!(i %in% lookup$real_names)) {
      if (i %in% lookup$fake_names) {
        # disallow this
        # -  to avoid confusion and
        # - to prevent anonymizing already anonymized names
        stop(paste(i, "is a real name but also used as a fake name in your 'lookup_file'."))
      }
      new_fake <- sample(x = names(good_names),
                         size = 1,
                         replace = FALSE,
                         prob = good_names)
      # now generate a completely fresh, never used fake name
      while (new_fake %in% c(lookup$real_names, lookup$fake_names, real_names)) {
        new_fake <- sample(x = names(good_names),
                           size = 1,
                           replace = FALSE,
                           prob = good_names)
      }
      lookup <- rbind(lookup, c(i, new_fake))
    }
  }

  utils::write.table(x = lookup,
                     sep = ",",
                     file = lookup_file,
                     row.names = FALSE,
                     append = FALSE)

  # let's be extra careful and validate the written file
  lookup_check <- utils::read.csv(file = lookup_file,
                                  header = TRUE,
                                  stringsAsFactors = FALSE,
                                  colClasses = c("character", "character"))
  expect_identical(object = lookup,
                   expected = lookup_check,
                   info = "Something went writing the 'lookup_file' to disk.")
  message(paste0("Lookup file written to ",
                 lookup_file,
                 "Keep it safe."))
  return(lookup[lookup[,"real_names"] %in% real_names, "fake_names"])
}
