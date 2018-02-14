# just for testing; never build objects like this by hand
good_obj <- structure(
  .Data = "I am an item.",
  class = c("psItemContentText", "psItemContent", "character"),
  markup = "plain"
)
bad_obj <- structure(
  .Data = 1L,
  class = c("psItemContentText", "psItemContent", "character") # must be character
)

check_S3(good_obj)
check_S3(bad_obj)
