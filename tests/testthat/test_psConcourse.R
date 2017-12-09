context("psConcourse")

# object construction ====
test_that(desc = "construction of multilingual text item works", code = {
  # TODO avoid this duplication, this is copied from examples
  multilingual_text <- psConcourse(
    concourse = matrix(
      data = c(
        "Man lives to work.", "Man lebt, um zu arbeiten.",
        "Man works to live.", "Man arbeitet, um zu leben."
      ),
      nrow = 2,
      ncol = 2,
      dimnames = list(items = c("live_2_work", "work_2_live"),
                      languages = c("english", "ngerman"))
    ),
    type = "text",
    markup = "plain",
    babel = TRUE
  )
  expect_s3_class(object = multilingual_text, class = c(
    "psConcourseText",
    "psConcourse",
    "matrix"))
})

test_that(desc = "construction of monolingual image item works", code = {
  # TODO avoid this duplication, this is copied from examples
  monolingual_image <- psConcourse(
    concourse = matrix(
      data = c("peach.jpg",
               "pear.jpg"),
      nrow = 2,
      ncol = 1,
      dimnames = list(
        items = c("peach", "pear"),
        languages = c("english")
     )),
   type = "image",
   img_dir = file.path(system.file(package = "pensieve"), "extdata", "fruit")
   # these files ship with pensieve
  )
  expect_s3_class(object = monolingual_image, class = c(
    "psConcourseImage",
    "psConcourse",
    "matrix"))
})
