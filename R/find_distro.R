find_distro <- function(data) {
  all_distros <- apply(X = data, MARGIN = 2, FUN = function(x) {count(x)})
  return(all_distros[[1]])
}
