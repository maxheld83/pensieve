## ----setup---------------------------------------------------------------
library(knitr)
library(rmarkdown)
library(pensieve)
install.packages(repos = NULL, type = "source", INSTALL_opts = c('--no-lock'), pkgs = c("../qmethod"))

## ----kill-nas------------------------------------------------------------
data(komki)
komki$qsorts <- na.omit(komki$qsorts)
komki$items <- komki$items[komki$items$Handle.english %in% rownames(komki$qsorts), ]
komki$qcat$ass <- sapply(X = komki$qcat$ass, simplify = FALSE, FUN = function(x) {
  x[rownames(x) %in% rownames(komki$qsorts), , drop = FALSE]
})
lapply(X = komki$qcat$ass, FUN = function(x) {is.matrix(x)})

## ----make-cooc-----------------------------------------------------------
cooc <- count_cooccur(ass = komki$qcat$ass)

## ----show-cooc-----------------------------------------------------------
kable(cooc[,,"Willy"])

