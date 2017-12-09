# multilingual study, text items
multilingual_text <- psConcourse(
  concourse = matrix(
    data = c(
      "Man lives to work.", "Man lebt, um zu arbeiten.",
      "Man works to live.", "Man arbeitet, um zu leben."
    ),
    nrow = 2,
    ncol = 2,
    dimnames = list(
      items = c("live_2_work", "work_2_live"),
      languages = c("english", "ngerman"))
  ),
  type = "text",
  markup = "plain",
  babel = TRUE
)

# monolingual study, image items ====
# this is a directory with some images that ship with pensieve
# location differs depending on runtime; ignore this
img_dir <- file.path("..", "..", "inst", "extdata", "fruit")
if (!dir.exists(img_dir)) {
  img_dir <- file.path(system.file(package = "pensieve"), "extdata", "fruit")
}

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
 img_dir = img_dir
)

# coerce matrix to psConcourse (multilingual concourse)
concourse <- matrix(
  data = c(
    "Man lives to work.",
    "Man lebt, um zu arbeiten.",
    "Man works to live.",
    "Man arbeitet, um zu leben."),
  nrow = 2,
  ncol = 2)
concourse <- as_psConcourse(
  concourse = concourse,
  languages = c("english", "ngerman"),
  handles = c("live_2_work", "work_2_live"))

# coerce data.frame to psConcourse (multilingual concourse)
concourse <- data.frame(
  english = c("man lives to work", "man works to live"),
  ngerman = c("man lebt, um zu arbeiten", "man arbeitet, um zu leben"))
as_psConcourse(concourse = concourse, handles = c("live_2_work", "work_2_live"))

# coerce character vector to psConcourse (monolingual concourse only)
concourse <- c(
  live_2_work = "man lives to work",
  work_2_live = "man works to live")
as_psConcourse(concourse, languages = "english")

#' # print concourse
knitr::knit_print(x = multilingual_text, use_js = TRUE, options = NULL)
