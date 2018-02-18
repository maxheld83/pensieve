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

validate_S3(good_obj)
validate_S3(bad_obj)
check_S3(good_obj)
check_S3(bad_obj)
test_S3(good_obj)
test_S3(bad_obj)
expect_S3(good_obj)
# expect_S3(bad_obj) # this errors out
assert_S3(good_obj)
# assert_S3(bad_obj)  # this errors out
need_S3(good_obj)
need_S3(bad_obj)


