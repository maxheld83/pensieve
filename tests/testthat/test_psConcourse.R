context("psConcourse")

# object construction ====
test_that(desc = "construction of multilingual text item works", code = {
  expect_s3_class(object = multilingual_text, class = c(
    "psConcourseText",
    "psConcourse",
    "matrix"))
})

test_that(desc = "construction of monolingual image item works", code = {
  expect_s3_class(object = monolingual_image, class = c(
    "psConcourseImage",
    "psConcourse",
    "matrix"))
})
