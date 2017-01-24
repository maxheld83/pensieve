# capitalize first letter of word in string ====
# useful for making S3 names
# from http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)),
        substring(s, 2),
        sep = "",
        collapse = " ")
}

# helper to append, and only if class not already in there
# this is to avoid duplicate classes, which is just confusing
classify_clever <- function(x, classname) {
  if (!inherits(x = x, what = classname)) {
    class(x) <- append(classname, class(x))
  }
  return(x)
}
