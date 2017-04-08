# This includes just the plotting functions, none of which are exported
# they are the workers behind the respective plotting *methods*

plot_heatmap <- function(color_matrix, color_title) {
  value <- x <- y <- Correlation <- NULL # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  # Data preparation ====
  df <- reshape2::melt(data = color_matrix, varnames = c("x", "y"))
  df$x <- factor(df$x)

  # Plotting ====
  g <- ggplot(data = df, mapping = aes(x = x, y = factor(y), fill = value, label = round(x = value, digits = 2)))
  g <- g + geom_tile()
  g <- g + scale_fill_gradient2(low = "red",
                                high = "blue",
                                mid = "white",
                                limits = c(-1, 1),  # make sure whole range of values is covered
                                name = color_title)
  g <- g + geom_text()
  g <- g + theme(axis.title = element_blank())  # kill axis labels
  if (isSymmetric(object = color_matrix)) {  # this only makes sense if object is symmetric, otherwise plenty of space
    g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))  # rotate x axis labels
  }
  return(g)
}


plot_barchart_length <- function(df, x, y, fill, flip) {
  g <- ggplot(data = df, mapping = aes_string(y = x, x = y, fill = fill))
  g <- g + geom_bar(stat = "identity", position = "dodge")  # but not if we're talking loadings
  g <- g + scale_y_continuous(limits = c(-1, 1), expand = c(0, 0))
  if (flip) {
    g <- g + coord_flip()
  }
  g <- g + theme(legend.position = "bottom")
  return(g)
}

plot_barchart_area <- function(df, x, y, fill, flip) {
  g <- ggplot(data = df, mapping = aes_string(y = x, x = y, fill = fill))
  g <- g + geom_bar(stat = "identity", width = 1)
  g <- g + scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
  g <- g + coord_flip()
  if (flip) {
    g <- g + coord_flip()
  }
  g <- g + theme(legend.position = "bottom")
  return(g)
}

plot_piechart_area <- function(df, x, y, fill) {
  g <- ggplot(data = df, mapping = aes_string(y = x, fill = fill, x = y))
  g <- g + geom_bar(stat = "identity", width = 1)
  g <- g + scale_y_continuous(limits = c(0, 1), trans = "sqrt")
  g + coord_polar(theta = "x")
  return(g)
  plotly::ggplotly(g)
}

plot_density <- function(df, x, color = NULL) {
  g <- ggplot(data = df, mapping = aes_string(x = x, color = color))

  # Extra stuff in case more linetypes (stat summaries) are added
  g <- g + geom_density(mapping = aes_string(y = "..density..", linetype = "Observed Data"))
  g <- g + scale_linetype_discrete(guide = FALSE)  # kill redundant label

  g <- g + xlim(-1, 1)
  g <- g + xlab(label = get_name(x))
  g <- g + scale_color_discrete(name = get_name(color))
  return(g)
}

plot_density_stacked <- function(df, x, fill = NULL) {
  g <- ggplot(data = df, mapping = aes_string(x = x, color = fill))
  g <- g + stat_ecdf(geom = "step", position = "identity")
  g <- g + scale_x_continuous(limits = c(0, 1), expand = c(0, 0), trans = "sqrt")
  g
}

# helper to get names
get_name <- function(x) {
  if (is.null(names(x))) {
    return(x)
  } else {
    return(names(x))
  }
}

