context("Anonymization")

lookup_file <- system.file("extdata",
                           "example_name_lookup.csv",
                           package = "pensieve")
real_names <- c("Hillary", "Barack", "George")

test_that(desc = "works with example",
          code = {
  fake_names <- anonymize(real_names = real_names,
                          lookup_file = lookup_file)
  expect_character(x = fake_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   len = 3,
                   unique = TRUE)
  expect_equal(object = fake_names, expected = c("Terrelle", "Shawnara", "Cesar"))
})

test_that(desc = "returns fake names in the order in which real names are provided",
          code = {
  fake_names <- anonymize(
    real_names = c("George", "Barack", "Hillary"),
    lookup_file = lookup_file)
  expect_equal(object = fake_names,
               expected = c("Cesar", "Shawnara", "Terrelle"))
})

test_that(desc = "appends fake names as necessary and writes them to file",
          code = {
  fake_names <- anonymize(real_names = c(real_names, "Marylin"),
                          lookup_file = system.file("extdata",
                                                    "example_name_lookup.csv",
                                                    package = "pensieve"))
  expect_character(x = fake_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   len = 4,
                   unique = TRUE)

  on_file <- read.csv(file = lookup_file)
  expect_equivalent(object = as.character(on_file[, "real_names"]), expected = c(real_names, "Marylin"))
  expect_equivalent(object = as.character(on_file[, "fake_names"]), expected = fake_names)
})

test_that(desc = "retains unused lookup table entries",
          code = {
  file.copy(from = lookup_file, to = "retain.temp.csv", overwrite = TRUE)
  fake_names <- anonymize(real_names = c("Hillary", "Barack"),
                          lookup_file = "retain.temp.csv")
  new_lookup <- read.csv("retain.temp.csv", stringsAsFactors = FALSE)
  old_lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)
  expect_equal(object = new_lookup, expected = old_lookup)
})
