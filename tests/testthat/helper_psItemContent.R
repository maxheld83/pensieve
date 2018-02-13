# text items with handles
items_text_en <- psItemContent(
  items = c(
    "live_2_work" = "Man lives to work.",
    "work_2_live" = "Man works to live."
  ),
  type = "text",
  markup = "plain",
  babel_language = "english"
)

# text items without handles
items_text_de <- psItemContent(
  items = c(
    "Man lebt um zu arbeiten.",
    "Man arbeitet, um zu leben."
  ),
  babel_language = "ngerman"
)

# text items without multilingual typographic support
items_text_esperanto <- psItemContent(
  items = c(
    "Viro vivas por labori.",
    "Viro laboras vivi."
  ),
  babel_language = NULL
)

# image items
# these images ship with pensieve
# location depends on runtime; ignore next three lines
img_dir <- file.path("..", "..", "inst", "extdata", "fruit")
if (!dir.exists(img_dir)) {
  img_dir <- file.path(system.file(package = "pensieve"), "extdata", "fruit")
}
items_image <- psItemContent(
  items = c("peach.jpg", "pear.jpg"),
  type = "image",
  img_dir = img_dir
)
