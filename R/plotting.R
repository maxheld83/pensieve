# This includes just the plotting functions, none of which are exported
# they are the workers behind the respective plotting *methods*

plot_heatmap <- function(color_matrix, color_title) {
  x <- y <- Correlation <- NULL # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  # Data preparation ====
  df <- reshape2::melt(data = color_matrix, value.name = "Correlation", varnames = c("x", "y"))

  # Plotting ====
  g <- ggplot(data = df, mapping = aes(x = x, y = y, fill = Correlation, label = round(x = Correlation, digits = 2)))
  g <- g + geom_tile()
  g <- g + scale_fill_gradient2(low = "red",
                                high = "blue",
                                mid = "white",
                                limits = c(-1, 1),  # make sure whole range of values is covered
                                name = color_title)
  g <- g + geom_text()
  g <- g + theme(axis.title = element_blank())  # kill axis labels
  g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))  # rotate x axis labels
  return(g)
}

plot_density <- function(cors, n_obs = NULL) {
  x <- y <- ..density.. <- coeff <- NULL # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  upper <- unclass(cors)[upper.tri(unclass(cors))]
  df <- reshape2::melt(data = upper, value.name = "coeff")
  g <- ggplot(data = df, mapping = aes(x = coeff))
  g <- g + geom_density(mapping = aes(y = ..density.., linetype = "Observed Data"))
  g <- g + xlim(-1,1)
  g <- g + xlab(label = "Correlation Coefficient")
  if (!is.null(n_obs)) {  # only add this curve, if it n_items is known
    g <- g + stat_function(fun = pearson_p, mapping = aes(linetype = "Random Data (Pearson's)"), args = list(n = n_obs))
    g <- g + scale_linetype_manual(values = c("Observed Data" = "solid",
                                              "Random Data (Pearson's)" = "dashed"),
                                   name = "Density Estimate in")
  } else {# kill redundant legend
    g <- g + scale_linetype_discrete(guide = FALSE)
  }
  return(g)
}
