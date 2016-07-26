#' @export
run_parallel <- function(data = NULL,  # raw data
                         n = NULL,  # number of variables, here PEOPLE-Variables
                         distro = NULL,  # distro, found from raw data if null
                         runs = 100,
                         centile = .95) {

  if (is.null(distro)) {
    distro <- find_distro(data = data)
  }
  if (is.null(n)) {
    n <- ncol(data)
  }

  spots <- rep(distro$x, distro$freq)  # these are all available spots

  # initialize empty matrix
  runmat <- matrix(data = NA,
                   nrow = runs,
                   ncol = nrow(data),
                   # notice that you can never have more non-zero EVs than observations, so this reduces below analysis A LOT
                   dimnames = list(run = NULL, eigenvalue = NULL))

  # progress indicator for loops
  if (interactive()) {
    pbtarget <- ""  # if this is run interactively, send pb to stdout (default)
  } else {
    pbtarget <- stderr()
  }
  pb <- txtProgressBar(min = 0,
                       max = runs,
                       initial = 0,
                       style = 3,
                       file = pbtarget)  # hack job to send this to console while knitting as per http://stackoverflow.com/questions/20009152/knitr-redirect-chunk-code-output-to-terminal

  library(parallel)
  library(foreach)
  library(doParallel)
  cores <- detectCores(logical = TRUE) #- 3  # leave one core free for other work

  cl <- parallel::makeCluster(spec = cores, outfile = pbtarget)
  registerDoParallel(cl)
  helperfuns <- ls()[sapply(ls(), function(x) is.function(get(x)))]

  runmat <- foreach(r = 1:runs, .export = helperfuns, .combine = "cbind") %dopar% { # loop over runs
    rdraw <- sapply(X = 1:n, FUN = function(x) {sample(x = spots, size = length(spots), replace = FALSE)})
    rpca <- prcomp(x = rdraw, scale. = TRUE, center = TRUE)$sdev^2
    return(rpca)
  }
  stopCluster(cl)

  res <- apply(X = runmat, MARGIN = 1, FUN = function(x) {quantile(x = x, probs = centile)})
  return(res)
}
