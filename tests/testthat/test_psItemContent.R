# s3 ====
context("psItemContent class")

# object construction
test_that(desc = "construction of text item works", code = {
  expect_s3_class(object = items_text_en, class = c(
    "psItemContentText",
    "psItemContent",
    "character"
  ))
  expect_character(x = items_text_de, len = 2)
  expect_subset(
    x = c("lang"),
    choices = names(attributes(items_text_en))
  )
  expect_named(
    object = items_text_en,
    expected = c("live_2_work", "work_2_live"),
    ignore.order = FALSE,
    ignore.case = FALSE
  )
})

test_that(desc = "construction of image item works", code = {
  expect_s3_class(object = items_image, class = c(
    "psItemContentBin",
    "psItemContent",
    "character"
  ))
  expect_subset(
    x = c("dir_bin"),
    choices = names(attributes(items_image))
  )
})

# md2latex ====
context("Conversion from markdown to LaTeX")

test_that(desc = "accepts by-hand LaTeX to override", code = {
  # something about the glue outputs requires as.character
  expect_equivalent(object = as.character(from_by_hand_latex$tex[[1]]), expected = as.character(by_hand_latex))
})
