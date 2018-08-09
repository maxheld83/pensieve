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

# subsetting ====
test_that(desc = "subsetting retains attributes", code = {
  expect_equal(object = attributes(items_text_de[1]), expected = attributes(items_text_de))
  expect_equal(object = attributes(items_image[1]), expected = attributes(items_image))
})

# knit_print method ====
test_that(desc = "knit_print returns proper S3 object", code = {
  knitted_items <- NULL
  knitted_items$named <- knit_print(x = items_text_en)
  knitted_items$unnamed <- knit_print(x = items_text_de)
  knitted_items$inline_named <- knit_print(x = items_text_en, inline = TRUE)
  knitted_items$inline_unnamed <- knit_print(x = items_text_de, inline = TRUE)
  purrr::iwalk(
    .x = knitted_items,
    .f = function(x, y) {
      expect_class(
        x = x,
        classes = c("knit_asis"),
        null.ok = FALSE,
        info = y
      )
      expect_character(
        x = x,
        any.missing = FALSE,
        null.ok = FALSE,
        info = y
      )
    }
  )
})
