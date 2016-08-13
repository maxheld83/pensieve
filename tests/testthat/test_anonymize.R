context("Anonymization")

test_that(desc = "works with example",
          code = {
  fake_names <- anonymize(real_names = c("Hillary", "Barack", "George"),
                          lookup_file = system.file("extdata",
                                                    "example_name_lookup.csv",
                                                    package = "pensieve"))
  expect_character(x = fake_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   len = 3,
                   unique = TRUE)
  })

test_that(desc = "appends fake names as necessary",
          code = {
  fake_names <- anonymize(real_names = c("Hillary", "Barack", "George", "Marylin"),
                          lookup_file = system.file("extdata",
                                                    "example_name_lookup.csv",
                                                    package = "pensieve"))
  expect_character(x = fake_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   len = 4,
                   unique = TRUE)
})
