context("Anonymization")

test_that(desc = "works with example",
          code = {
  fake_names <- anonymize(real_names = c("Hillary", "Barack", "George"),
                          lookup_file = system.file("extdata",
                                                    "example_name_lookup.csv",
                                                    package = "pensieveR"))
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
                                                    package = "pensieveR"))
  expect_character(x = fake_names,
                   any.missing = FALSE,
                   all.missing = FALSE,
                   len = 4,
                   unique = TRUE)
})

test_that(desc = "errors out on real names that are also fake names",
          code = {
  file <- data.frame(real_names = c("Hillary", "Barack"),
                     fake_names = c("Hillary", "John"),
                     stringsAsFactors = FALSE)
  write.csv(x = file, file = "test.temp.csv", row.names = FALSE)
  expect_error(object = anonymize(real_names = c("Hillary", "Barack"),
                                  lookup_file = "test.temp.csv"))
})
