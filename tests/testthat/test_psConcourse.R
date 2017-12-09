context("psConcourse")

# object construction ====
test_that(desc = "construction of multilingual text item works", code = {
  expect_s3_class(object = multilingual_text, class = c(
    "psConcourseText",
    "psConcourse",
    "matrix"))
  expect_subset(
    x = c("markup", "babel"),
    choices = names(attributes(multilingual_text)))
  expect_named(
    object = dimnames(multilingual_text),
    expected = c("items", "languages"),
    ignore.order = FALSE,
    ignore.case = FALSE)
})

test_that(desc = "construction of monolingual image item works", code = {
  expect_s3_class(object = monolingual_image, class = c(
    "psConcourseImage",
    "psConcourse",
    "matrix"))
  expect_subset(
    x = c("img_dir"),
    choices = names(attributes(monolingual_image)))
})

test_that(desc = "coercion from matrix works", code = {
  expect_equal(object = from_matrix, expected = multilingual_text)
})

test_that(desc = "coercion from df works", code = {
  expect_equal(object = from_df, expected = multilingual_text)
})

test_that(desc = "coercion from vector works", code = {
  expect_equal(object = from_vec, expected = monolingual_text)
})
