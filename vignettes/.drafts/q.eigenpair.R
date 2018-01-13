Eigen.Illustrate <- function(data, cormatrix = NULL, pairs = NULL, cases = NULL) {
  # this is a function to generate some visualizations for some example eigenvalue decompositions (PCA) for real data
  # this works only for a PAIR of variables (2), so it's not the real deal of PCA, just to illustrate what PCA does, especially in the context of Q

  # Input validation =============
    # TODO(maxheld83) complete data validation
  if (!is.null(pairs)) {
    # pairs can be a (named) list of char vectors, each of length 2
    if (!is.list(pairs)) {
      stop("The specified pairs are not a list.")
    }
    if (!all(is.vector(pairs) | length(pairs) == 2)) {  # check if all ar vectors of length 2
      stop("At least one of the list members are not vectors of length 2.")
    }
  }
  if (!is.null(cases)) {
    if (!is.vector(cases)) {
      stop("The specified cases are not a vector.")
    }
    if (!all(cases %in% rownames(data))) {
      stop("At least one of the vector elements specified for cases is not a rowname in the data.")
    }
  }

  # Setting up vars ==========
  if (is.null(cormatrix)) {
    cormatrix <- cor(x = data, method = "spearman")  # calculate correlation matrix
  }

  if (is.null(pairs)) {
    # Make pairs
    pairs <- vector("list", 4)  # create empty list
    names(pairs) <- c("positive", "negative", "lowest", "identity")
    pairs$positive <- rownames(which(cormatrix == max(cormatrix[cormatrix < 1]), arr.ind = TRUE))  # find the highest positive correlation pair
    pairs$negative <- rownames(which(cormatrix == min(cormatrix[cormatrix < 1]), arr.ind = TRUE))  # find the highest negative correlation pair
    pairs$lowest <- rownames(which(abs(cormatrix) == min(abs(cormatrix[cormatrix < 1])), arr.ind = TRUE))  # find the lowest correlation
    pairs$identity <- c(rep(x = pairs$lowest[2], times = 2))  # identity is just one with him/herself
  }

  # Sample item labels (using all would hopelessly overlopt)
  if (is.null(cases)) {
    #TODO(maxheld83) maybe later program something that actually selects interesting cases -> https://github.com/maxheld83/schumpermas/issues/310
    cases <- row.names(data)[sample(x = 1:length(row.names(data)), size = 2, replace = FALSE )]
  }

  # Set up general plot characteristics
  styleg <- function(g) {
    g <- g + geom_point(alpha = 1/3)  # alpha b/c of overplotting
    g <- g + coord_fixed()  # otherwise, the angles of all vectors are off
    g <- g + ggtitle(i)
    g <- g + theme(legend.position = "bottom")
    return(g)
  }

  # Set up data structure
  data <- as.data.frame(data)  # ggplot does not like matrices
  eigenpairs <- matrix(data = pairs, nrow = length(pairs), ncol = 11, dimnames = list(names(pairs), c("pairs", "scatterplot", "eigen", "eigenplot", "pca", "biplot", "evector", "evalue", "corrm", "scalem", "formula")))
    # make this a MATRIX of LISTS (though pairs is just a vector, other cbound objects will be lists)

  # For loop over all pairs
  for (i in row.names(eigenpairs)) {

    # scatterplot ========
    current.pair <- eigenpairs[[i,"pairs"]]
    # this is the part as documented on http://stats.stackexchange.com/q/153564/60119
    g <- NULL  # just to make sure we're not carrying stuff from other loops
    g <- ggplot(data = data[,current.pair], mapping = aes_string(x = current.pair[1], y = current.pair[2], label = deparse(row.names(data))))
    g <- g + geom_text(data = data[cases, current.pair], mapping = aes_string(x = current.pair[1], y = current.pair[2], label = deparse(row.names(data[cases, current.pair]))))
    scatterplot <- g + geom_abline(intercept = 0, slope = cor(data[, current.pair], method = "spearman")[2], mapping = aes(colour = "Spearman"), show_guide = TRUE)  # plot spearman
    scatterplot <-  scatterplot + geom_abline(intercept = 0, slope = cor(data[, current.pair], method = "pearson")[2], mapping = aes(colour = "Pearson"), show_guide = TRUE)  # plot spearman
    scatterplot <- scatterplot + scale_colour_manual(values = c("blue","pink"), labels = c("Spearman", "Pearson"), name = "Correlation Coefficient")
    scatterplot <- styleg(g = scatterplot)
    eigenpairs[[i,"scatterplot"]] <- scatterplot

    # save whatever cormatrix was entered (this CAN be spearman!)
    corrm <- cormatrix[current.pair,current.pair]
    eigenpairs[[i, "corrm"]] <- corrm  # write to output object
    # calculate eigenstuff ====
    eigen <- eigen(x = corrm)  # calculate eigenvectors and values
    eigenpairs[[i,"eigen"]] <- eigen  # keep this for legacy support
    evalue <- eigen$values
    eigenpairs[[i, "evalue"]] <- evalue  # store sep for convenience
    evector <- as.matrix(eigen$vectors[,1])  # take only 1st eigenvector
    eigenpairs[[i, "evector"]] <- evector  # store sep for convenience
    scalem <- corrm %*% as.matrix(evector)
    eigenpairs[[i, "scalem"]] <- scalem  # store sep for convenience

    # formula ===
    source(file = "R/Make.LaTeX.Matrix.R")  # this is to make the latex
    formula <- c(
      "\\begin{aligned}",  # set up environment
      # formula with placeholders
      "\\mathbf{Correlation}_{", current.pair, "}", "\\times", "\\mathbf{Eigenvector}",
      "=",
      "\\mathbf{Scaled Matrix}",
      "=",
      "Eigenvalue", "\\times", "\\mathbf{Eigenvector}",
      "\\\\",  # linebreak
      # with actual data
      m2l(corrm), "\\times", m2l(evector),
      "=",
      m2l(scalem),
      "=", round(x = as.vector(unique(scalem / evector)), digits = 2), "\\times", m2l(evector),  # the multiplier
      "\\end{aligned}"
    )
    eigenpairs[[i, "formula"]] <- formula  # store sep for convenience

    # eigenplot ======
    if (current.pair[1] != current.pair[2]) {  # ellipse is only defined if not equal, makes sense!
      g <- g + stat_ellipse(type = "norm")
      # add ellipse, though I am not sure which is the adequate type
      # as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
    }
    eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]  # calc slopes as ratios
    eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]  # calc slopes as ratios
    g <- g + geom_abline(intercept = 0, slope = eigen$slopes[1], mapping = aes(colour = "PC1"), show_guide = TRUE)  # plot pc1
    if (eigen$values[2] > 0) {  # second axis sense only if there is, in fact, a value
      g <- g + geom_abline(intercept = 0, slope = eigen$slopes[2], mapping = aes(colour = "PC2"), show_guide = TRUE)  # plot pc2
    }
    g <- g + scale_colour_manual(values = c("green","red"), labels = c("PC1", "PC2"), name = "Principal Components")
    g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[1], yend = eigen$slopes[1] * eigen$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
    g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[2], yend = eigen$slopes[2] * eigen$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2
    # Here come the perpendiculars, from StackExchange answer http://stackoverflow.com/questions/30398908/how-to-drop-a-perpendicular-line-from-each-point-in-a-scatterplot-to-an-eigenv ===
    perp.segment.coord <- function(x0, y0, a=0, b=1){
      #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
      # defined by lm.mod as y=a+b*x
      x1 <- (x0 + b * y0 - a * b) / (1 + b ^ 2)
      y1 <- a + b * x1
      list(x0 = x0, y0 = y0, x1 = x1, y1 = y1)
    }
    ss <- perp.segment.coord(data[, current.pair[1]], data[, current.pair[2]], 0, eigen$slopes[1])
    g <- g + geom_segment(data = as.data.frame(ss), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "green", linetype = "dotted")
    eigenplot <- g
    eigenplot <- styleg(g = eigenplot)
    eigenpairs[[i,"eigenplot"]] <- eigenplot  # assign output

    # Make PCA ====
    pca <- princomp(x = data[, current.pair], scores = TRUE, cor = TRUE)
    #TODO(maxheld83) notice that this, sadly, produces the pca based on the pearson correlation coefficient, which cannot be easily changed. No currently available package produces scores AND is based on another cor method. https://github.com/maxheld83/schumpermas/issues/312
    eigenpairs[[i,"pca"]] <- pca  # assign output

    # Make biplot =======
    pca$scores <- as.data.frame(pca$scores)  # ggplot does not like arrays
    pca$loadings <- as.data.frame(unclass(pca$loadings))  # ggplot does not like classed objects either
    pca$loadings$variable <- row.names(pca$loadings)  # stupid but necessary, ggplot2 otherwise craps out on missing object
    g <- NULL  # start from scratch
    g <- ggplot(data = pca$scores, mapping = aes(x = Comp.1, y = Comp.2))
    g <- g + ylim(-max(abs(pca$scores)), max(abs(pca$scores)))
    g <- g + xlim(-max(abs(pca$scores)), max(abs(pca$scores)))
    g <- g + geom_segment(data = pca$loadings, mapping = aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2, colour = variable), arrow = arrow(length = unit(0.2, "cm")))
    g <- g + scale_colour_manual(values = c("yellow", "orange"), name = "Variable")  # add arrows for original cars
    #    g <- g + geom_text(data = pca$scores[cases,], mapping = aes(x = Comp.1, y = Comp.2, label = row.names(pca$scores[cases, ])))
    biplot <- g
    biplot <- styleg(g = biplot)
#     vec.examples["positive",4]
#     row.names(pca$loadings)
#     str(pca$loadings)
#     otherpca <- prcomp(x = data[,current.pair])
#     str(otherpca$rotation)
#     pca$loadings
#     vec.examples["positive",4]
#     pca$scores
#     ggbiplot(pca)
#     g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[1], yend = eigen$slopes[1] * eigen$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
#     g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[2], yend = eigen$slopes[2] * eigen$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2
#     g
#     if (current.pair[1] != current.pair[2]) {  # if this is NOT about identity
#
#       # fviz_pca_biplot(X = pca, addEllipses = TRUE) #alternative plotter
#       g <- ggbiplot(pcobj = pca, obs.scale = TRUE, var.scale = TRUE, labels = row.names(pca$scores))
#       g
#       g <- g + ylim(-max(abs(pca$scores)), max(abs(pca$scores)))
#       g <- g + xlim(-max(abs(pca$scores)), max(abs(pca$scores)))
#       pca$scores <- as.data.frame(pca$scores)  # because labeling does not like array
#       g <- g + geom_text(data = pca$scores[cases, ], mapping = aes(x = Comp.1, y = Comp.2, label = rownames(pca$scores[cases, ])))
#     } else { # this is the hackjob
#       pca.id <- prcomp(x = data[, current.pair])  # for some reason princomp won't work for the plot. Careful though; the results in the pca object and this one are out of wack! -> https://github.com/maxheld83/schumpermas/issues/311
#       g <- NULL
#       g <- ggbiplot(pcobj = pca.id)  # these extra functions don't work for identity
#       #g <- g + ylim(-max(abs(pca.id$x)), max(abs(pca.id$x)))
#       #g <- g + xlim(-max(abs(pca.id$x)), max(abs(pca.id$x)))
#       pca.id$x <- as.data.frame(pca.id$x)  # because labeling does not like array
#       g <- g + geom_text(data = pca.id$x[cases, ], mapping = aes(x = PC1, y = PC2,  label = rownames(pca.id$x[cases, ])))
#     }
#     biplot <- g
    eigenpairs[[i, "biplot"]] <- biplot  # assign output
  }
  return(eigenpairs)
}
