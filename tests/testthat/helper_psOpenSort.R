# create single open sort ====

# Lisas open sort, unnamed descriptions (matched by index)
osort <- matrix(
  data = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE),
  nrow = 3,
  dimnames = list(items = c("cat", "dog", "cow")))
descriptions <- c(
  "a pet which largely takes care of itself",
  NA  # dimension is assigned, but not described (not a problem)
)
lisa <- psOpenSort(osort = osort, descriptions = descriptions)

# Peters open sort, named descriptions (*also* only matched by index)
osort <- matrix(
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
peter <- psOpenSort(osort = osort, descriptions = descriptions)

# coercion methods
peter_m <- as_psOpenSort(osort = as.matrix(x = osort), descriptions = descriptions)
peter_df <- as_psOpenSort(osort = as.data.frame(x = osort), descriptions = descriptions)

# Rebeccas open sort, without any descriptions provided
osort <- matrix(
  data = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
  nrow = 3,
  dimnames = list(handles = c("cat", "dog", "cow")))
rebecca <- psOpenSort(osort = osort, descriptions = NULL)
