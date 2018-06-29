# TODO shorten this, or reuse from somewhere else!
items_text_en <- psItemContent(
  items = c(
    "live_2_work" = "Man lives to work.",
    "work_2_live" = "Man works to live."
  ),
  type = "text",
  lang = "en-US"
)
grid <- matrix(data = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), nrow = 2)
grid <- as_psGrid(grid)
sort <- matrix(
  data = c(NA, "live_2_work", NA, "work_2_live", NA, NA),
  nrow = 2)
sort <- psSort(
  sort = sort,
  grid = grid,
  items = items_text_en,
  pattern = "chessboard",
  offset = NULL)
