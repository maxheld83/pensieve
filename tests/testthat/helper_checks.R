# just for testing; never build objects like this by hand
good_obj <- structure(.Data = "I am an item.", class = "psItemContent")
bad_obj <- structure(.Data = 1L, class = "psItemContent") # must be character

check_S3(good_obj)
check_S3(bad_obj)
