# create single open sort ====

# Lisas open sort, unnamed descriptions (matched by index)
losort <- matrix(
  data = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
  nrow = 3,
  dimnames = list(items = c("cat", "dog", "cow")))
descriptions <- c(
  "a pet which largely takes care of itself",
  NA  # dimension is assigned, but not described (not a problem)
)
lisa <- psOpenSort(osort = losort, descriptions = descriptions)

# Peters open sort, named descriptions (*also* only matched by index)
losort <- matrix(
  data = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
  nrow = 3,
  dimnames = list(
    items = c("cat", "dog", "cow"),
    categories = c("in_homes", "quiet", "herbivore")
  ))
descriptions <- c(
  in_homes = "Animal found in peoples homes.",
  quiet = "Does not make a lot of noise.",
  herbivore = "Eats plants.")  # defined, but never TRUE (not a problem)
peter <- psOpenSort(osort = losort, descriptions = descriptions)

# coercion methods
peter_m <- as_psOpenSort(osort = as.matrix(x = losort), descriptions = descriptions)
peter_df <- as_psOpenSort(osort = as.data.frame(x = losort), descriptions = descriptions)

# Rebeccas open sort, without any descriptions provided
losort <- matrix(
  data = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
  nrow = 3,
  dimnames = list(handles = c("cat", "dog", "cow")))
rebecca <- psOpenSort(osort = losort, descriptions = NULL)

# Ira open sort, with some problems
losort <- matrix(
  data = c(
    FALSE, FALSE, FALSE,  # this is dropped, b/c there is just no valuable information here,
    TRUE, TRUE, TRUE,  # same problem; no variance
    FALSE, FALSE, FALSE,
    # also no variance, but there *is* a corresponding description,
    # so we're setting column to NA and keeping the description
    NA, TRUE, FALSE),  # you can also have *actual* NAs
  nrow = 3,
  byrow = FALSE,
  dimnames = list(handles = c("cat", "dog", "cow"))
)
descriptions <- c(NA, NA, "mammals", NA)
ira <- suppressWarnings(as_psOpenSort(osort = losort, descriptions = descriptions))
# this gives appropriate warning messages
# psOpenSort() would error out; only coercion method will attempt fix

# ordinally and intervally scaled sorts are also possible, but currently unsupported
tyler <- matrix(
  data = as.integer(c(1, 2, 2, 1)),
  nrow = 2,
)
tyler <- psOpenSort(
  osort = tyler,
  scale = "ordinal")  # defaults to implicit class of base type
roberta <- matrix(
  data = c(2.2, 4.3, -2.8, 0),
  nrow = 2
)
roberta <- psOpenSort(osort = roberta)

