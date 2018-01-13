# With borders ===
m2lb <- function(matr) {  # this is the function that includes borders as row/colnames
  matr <- round(x = matr, digits = 2)  # sadly this is necessary because given this function, the options(digits = 2) does not work
  matr2 <- data.frame(c("~",rownames(matr)))  # add rownames
  for (r in colnames(matr)) {  # add col contents and colnames
    matr2 <- cbind(matr2, c(r, matr[,r]))
  }
  printmrow <- function(x) {
    ret <- paste(paste(x, collapse = " & "), "\\cr")
    sprintf(ret)
  }
  out <- apply(matr2, 1, printmrow)
  out2 <- paste("\\bordermatrix{", paste(out, collapse = ' '),"}")
  return(out2)
}
# Without borders ===
m2l <- function(matr) {  # this function only makes matrices, without borders
  # this is from http://stackoverflow.com/questions/20759444/knitr-r-code-within-latex-environment-in-a-markdown-document
  matr <- round(x = matr, digits = 2)  # sadly this is necessary because given this function, the options(digits = 2) does not work
  printmrow <- function(x) {
    ret <- paste(paste(x, collapse = " & "), "\\\\")
    sprintf(ret)
  }
  out <- apply(matr, 1, printmrow)
  out2 <- paste("\\begin{pmatrix}", paste(out, collapse = ' '), "\\end{pmatrix}")  # use parantheses instead
  return(out2)
}
