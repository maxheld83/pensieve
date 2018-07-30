# text items with handles
items_text_en <- psItemContent(
  items = c(
    "live_2_work" = "Man lives to work.",
    "work_2_live" = "Man works to live."
  ),
  type = "text",
  lang = "en-US"
)

# text items without handles
items_text_de <- psItemContent(
  items = c(
    "Man lebt um zu arbeiten.",
    "Man arbeitet, um zu leben."
  ),
  lang = "de-DE"
)

# text items without multilingual typographic support
items_text_esperanto <- psItemContent(
  items = c(
    "Viro vivas por labori.",
    "Viro laboras vivi."
  ),
  lang = NULL
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

# rendering text items ====
rendered_items <- render_items(items = items_text_en, lang = "en-US")


# supplying your own compiled LaTeX
by_hand_latex <- glue::glue(.open = "[", .close = "]", "
  \\documentclass{article}
  \\begin{document}

  Let's say this is an item.

  \\end{document}
")
from_by_hand_latex <- render_items(
  items = "Let's say this is an item.",
  # full item wording should still be provided for other uses!
  tex = list(by_hand_latex)
)
