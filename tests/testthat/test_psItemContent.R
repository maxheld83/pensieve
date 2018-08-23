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
  expect_equal(
    object = get_attributes_but(x = items_text_de[1], not_attrs = "names"),
    expected = get_attributes_but(x = items_text_de, not_attrs = "names")
  )
  expect_equal(
    object = get_attributes_but(x = items_image[1], not_attrs = "names"),
    expected = get_attributes_but(x = items_image, not_attrs = "names")
  )
})

# knit_print method ====
test_that(desc = "knit_print returns proper S3 object", code = {
  skip_on_appveyor()
  skip_on_dev_machine()
  knitted_items <- NULL
  knitted_items$named <- knit_print(x = items_text_en)
  knitted_items$unnamed <- knit_print(x = items_text_de)
  knitted_items$inline_named <- knit_print(x = items_text_en, inline = TRUE)
  knitted_items$inline_unnamed <- knit_print(x = items_text_de, inline = TRUE)
  iwalk(
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


# export method ====
# setwd(dir = "tests/testthat/") # for interactive testing
path <- fs::path("test_psItemContent")
setup(code = {
  fs::dir_create(path = path)
})
teardown(code = {
  # comment me out to debug results
  fs::dir_delete(path = path)
})

test_that(desc = "exported items are 1 page only", code = {
  skip_on_appveyor()
  skip_on_dev_machine()
  test_items <- psItemContent(items = c(short = "short", long = glue_collapse(rep("long", times = 60), sep = " ")))
  paths <- export_ps(x = test_items, dir = path, overwrite = TRUE)
  expect_pdf1page(x = paths["long"])
})

test_that(desc = "export method writes files to all formats", code = {
  skip_on_appveyor()
  skip_on_dev_machine()
  walk(
    .x = names(render_chain_formats)[-4],
    .f = function(x) {
      files <- export_ps(x = items_text_en, dir = path, format = x)
      expect_file_exists(x = files, access = "r", extension = x)
    }
  )
})
