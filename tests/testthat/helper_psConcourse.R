# multilingual study, text items ====
m <- matrix(
  data = c(
    "Man lives to work.", "Man lebt, um zu arbeiten.",
    "Man works to live.", "Man arbeitet, um zu leben."),
  nrow = 2,
  byrow = TRUE)

# helper expects handles as rownames, languages as colnames
# see below coercion for easier input
concourse <- m
colnames(concourse) <- c("english", "ngerman")  # "ngerman" is a babel lang
rownames(concourse) <- c("live_2_work", "work_2_live")

multilingual_text <- psConcourse(
  concourse = concourse
)

# for monolingual concourses, just use one column
monolingual_text <- psConcourse(
  concourse = concourse[,"english", drop = FALSE]
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

# coercion methods ====

# coercion from matrix
# aka providing handles and languages as arguments
from_matrix <- as_psConcourse(
  concourse = m,
  languages = c("english", "ngerman"),
  handles = c("live_2_work", "work_2_live"))

# coercion from data.frame
from_df <- as_psConcourse(concourse = data.frame(concourse))

# coercion from (named) vector (monolingual concourse only)
vec <- concourse[,"english"]
from_vec <- as_psConcourse(vec, languages = "english")


# printing methods ====

# print in knitr chunks
# also works in RStudio interactively
knitr::knit_print(x = multilingual_text, use_js = TRUE, options = NULL)
