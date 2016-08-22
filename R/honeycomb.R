count_hexcells <- function(n_circles) {
  1 + 6 * (1/2 * (n_circles * (n_circles - 1)))
}
#TODO make tests!
#count_hexcells(3) == 9

count_range <- function(n_circles) {
  1 + (n_circles - 1) * 4
}
#count_range(4)  == 13

find_longdiag_from_card <- function(cardwidth, cardheight, rotation = "oncorner") {
  if (rotation == "oncorner") {

    # first we test whether the rectangle is so slim as to be only determined by its width
    flat_rectangle <- sqrt(3) * cardheight < cardwidth

    if (flat_rectangle) {
      shortdiag <- cardwidth  # this is the simple case
      #shortdiag is d2 as per https://rechneronline.de/pi/sechseck.php
      a <- shortdiag/sqrt(3)
      d <- 2 * a
      longdiag <- d
    } else {
      a <- cardwidth/2 * (sin(30*pi/180)/sin(60*pi/180))
      longdiag <- 2*a + cardheight
    }
  } else {
    stop("Only 'oncorner' rotation is implemented.")
  }
  return(longdiag)
}

find_height <- function(n_circles, longdiag) {
  nrows <- (n_circles * 2) - 1
  n_diag <- (nrows + 1) / 2
  # this is the number of cells, where we need to take the long diagonal
  n_sidelength <- (nrows - 1) / 2
  # number of cells where we need to add the sidelength
  sidelength <- longdiag / 2
  return(n_diag * longdiag + n_sidelength * sidelength)
}

find_width <- function(n_circles, longdiag) {
  ncols <- (n_circles * 2) - 1
  shortdiag <- longdiag / 2 * sqrt(3)
  return(ncols * shortdiag)
}

find_all <- function(n_circles, cardwidth, cardheight) {
  # wrapper function
  longdiag <- find_longdiag_from_card(cardwidth = cardwidth, cardheight = cardheight)
  return(list(
    longdiag = longdiag,
    width = find_width(n_circles = n_circles, longdiag = longdiag),
    height = find_height(n_circles = n_circles, longdiag = longdiag),
    items = as.integer(count_hexcells(n_circles = n_circles)),
    range = as.integer(count_range(n_circles = n_circles))))
}

find_all(n_circles = 5, cardwidth = 8.5, cardheight = 5.4)
