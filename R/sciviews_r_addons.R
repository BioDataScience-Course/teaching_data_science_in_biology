SciViews::R()


# Addons for PCA ----------------------------------------------------------

library(broom)

# broom implements only methods for prcomp objects, not princomp, while pcomp
# is compatible with princomp... but prcomp is simpler. So, conversion is easy
as.prcomp <- function(x, ...)
  UseMethod("as.prcomp")

as.prcomp.default <- function(x, ...)
  stop("No method to convert this object into a 'prcomp'")

as.prcomp.prcomp <- function(x, ...)
  x

as.prcomp.princomp <- function(x, ...)
  structure(list(sdev = as.numeric(x$sdev), rotation = unclass(x$loadings),
    center = x$center, scale = x$scale, x = as.matrix(x$scores)),
    class = "prcomp")

# Comparison of pcomp() -> as.prcomp() with prcomp() directly
# Almost the same, only no rownames for x (is it important?)
#iris_prcomp_pcomp <- as.prcomp(pcomp(iris[, -5], scale = TRUE))
#iris_prcomp <- prcomp(iris[, -5], scale = TRUE)

# Now, broom methods can be defined simply by converting into prcomp objects
augment.princomp <- function(x, data = NULL, newdata, ...)
  if (missing(newdata)) {
    augment(as.prcomp(x), data = data, ...)
  } else {
    augment(as.prcomp(x), data = data, newdata = newdata, ...)
  }

tidy.princomp <- function(x, matrix = "u", ...)
  tidy(as.prcomp(x), matrix = matrix, ...)

# There is no glance.prcomp() method

# There is a problem with pcomp() that returns a data.frame in scores,
# while it is a matrix in the original princomp object. pca() corrects this
pca <- function(x, ...) {
  res <- SciViews::pcomp(x, ...)
  # Change scores into a matrix
  res$scores <- as.matrix(res$scores)
  res
}

scale_axes <- function(data, aspect.ratio = 1) {
  range_x <- range(data[, 1])
  span_x <- abs(max(range_x) - min(range_x))
  range_y <- range(data[, 2])
  span_y <- abs(max(range_y) - min(range_y))
  if ((span_y / aspect.ratio) > span_x) {
    # Adjust range_x
    span_x_2 <- span_y / aspect.ratio / 2
    range_x_mid <- sum(range_x) / 2
    range_x <- c(range_x_mid - span_x_2, range_x_mid + span_x_2)
  } else {
    # Adjust range_y
    span_y_2 <- span_x * aspect.ratio / 2
    range_y_mid <- sum(range_y) / 2
    range_y <- c(range_y_mid - span_y_2, range_y_mid + span_y_2)
  }
  list(x = range_x, y = range_y)
}

autoplot.pcomp <- function(object,
  type = c("screeplot", "altscreeplot", "loadings", "correlations", "scores", "biplot"),
  choices = 1L:2L, name = deparse(substitute(object)), ar.length = 0.1,
  circle.col = "gray", col = "black", fill = "gray", scale = 1, aspect.ratio = 1,
  repel = FALSE, labels, title, xlab, ylab, ...) {
  type = match.arg(type)

  if (missing(title))
    title <- paste(name, type, sep = " - ")

  contribs <- paste0(names(object$sdev), " (",
    round((object$sdev^2/object$totdev^2) * 100, digits = 1), "%)")[choices]

  scores <- as.data.frame(object$scores[, choices])
  names(scores) <- c("x", "y")
  if (!missing(labels)) {
    if (length(labels) != nrow(scores))
      stop("You must provide a character vector of length ", nrow(scores),
        " for 'labels'")
    scores$labels <- labels
  } else {# Default labels are row numbers
    scores$labels <- 1:nrow(scores)
  }

  lims <- scale_axes(scores, aspect.ratio = aspect.ratio)

  if (!missing(col)) {
    if (length(col) != nrow(scores))
      stop("You must provide a vector of length ", nrow(scores), " for 'col'")
    scores$color <- col
    scores_formula <- y ~ x %col=% color %label=% labels
  } else {
    if (missing(labels)) {
      scores_formula <- y ~ x %label=% labels
    } else {
      scores_formula <- y ~ x %col=% labels %label=% labels
    }
  }

  res <- switch(type,
    screeplot = object %>.% # Classical screeplot
      tidy(., "pcs") %>.%
      chart(data = ., std.dev^2 ~ PC) +
      geom_col(col = col, fill = fill) +
      labs(y = "Variances", title = title),

    altscreeplot = object %>.% # screeplot represented by dots and lines
      tidy(., "pcs") %>.%
      chart(data = ., std.dev^2 ~ PC) +
      geom_line(col = col) +
      geom_point(col = "white", fill = col, size = 2, shape = 21, stroke = 3) +
      labs(y = "Variances", title = title),

    loadings = object %>.% # Plots of the variables
      tidy(., "variables") %>.%
      spread(., key = PC, value = value) %>.%
      #rename_if(., is.numeric, function(x) paste0("PC", x)) %>.%
      select(., c(1, choices + 1)) %>.%
      set_names(., c("labels", "x", "y")) %>.%
      chart(data = ., y ~ x %xend=% 0 %yend=% 0 %label=% labels) +
      annotate("path", col = circle.col,
        x = cos(seq(0, 2*pi, length.out = 100)),
        y = sin(seq(0, 2*pi, length.out = 100))) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      geom_segment(arrow = arrow(length = unit(ar.length, "inches"),
        ends = "first")) +
      ggrepel::geom_text_repel(hjust = "outward", vjust = "outward") +
      coord_fixed(ratio = 1) +
      labs(x = contribs[1], y = contribs[2], title = title),

    correlations = object %>.% # Correlations plot
      Correlation(.) %>.%
      as_tibble(., rownames = "labels") %>.%
      select(., c(1, choices + 1)) %>.%
      set_names(., c("labels", "x", "y")) %>.%
      chart(data = ., y ~ x %xend=% 0 %yend=% 0 %label=% labels) +
      annotate("path", col = circle.col,
        x = cos(seq(0, 2*pi, length.out = 100)),
        y = sin(seq(0, 2*pi, length.out = 100))) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      geom_segment(arrow = arrow(length = unit(ar.length, "inches"),
        ends = "first")) +
      ggrepel::geom_text_repel(hjust = "outward", vjust = "outward") +
      coord_fixed(ratio = 1) +
      labs(x = contribs[1], y = contribs[2], title = title),

    scores = scores %>.% # Plot of the individuals
      chart(data = ., scores_formula) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      coord_fixed(ratio = 1, xlim = lims$x, ylim = lims$y, expand = TRUE) +
      labs(x = contribs[1], y = contribs[2], title = title) +
      theme(legend.position = "none"),

    biplot = object %>.% # Biplot using ggfortify function
      as.prcomp(.) %>.%
      ggfortify:::autoplot.prcomp(., x = choices[1], y = choices[2],
        scale = scale, size = -1, label = TRUE, loadings = TRUE,
        loadings.label = TRUE) +
      geom_hline(yintercept = 0, col = circle.col) +
      geom_vline(xintercept = 0, col = circle.col) +
      theme_sciviews() +
      labs(x = contribs[1], y = contribs[2], title = title),

    stop("Unrecognized type, must be 'screeplot', 'altscreeplot', loadings', 'correlations', 'scores' or 'biplot'")
  )

  if (type == "scores") {
    if (isTRUE(repel)) {
      res <- res + geom_point() + ggrepel::geom_text_repel()
    } else {# Use text
      res <- res + geom_text()
    }
  }

  if (!missing(xlab))
    res <- res + xlab(xlab)
  if (!missing(ylab))
    res <- res + ylab(ylab)
  res
}

chart.pcomp <- function(data, choices = 1L:2L, name = deparse(substitute(data)),
  ..., type = NULL, env = parent.frame())
  autoplot.pcomp(data, choices = choices, name = name, ..., type = type, env = env)
class(chart.pcomp) <- c("function", "subsettable_type")


# CAH ---------------------------------------------------------------------

# CAH for SciViews, version 1.2.0
# Copyright (c) 2021, Philippe Grosjean (phgrosjean@sciviews.org)

SciViews::R

# dist is really a dissimilarity matrix => we use dissimilarity() as in the
# {cluster} package, i.e., class is c("dissimilarity", "dist")
# TODO: also make a similarity object and convert between the two
# fun can be stats::dist, vegan::vegdist, vegan::designdist, cluster::daisy
# factoextra::get_dist and probably other dist-compatible functions
# Depending on method =, use either vegan::vegdist or stats::dist as default fun
dissimilarity <- function(data, formula = ~ ., subset = NULL,
  method = "euclidean", scale = FALSE, rownames.col = "rowname",
  transpose = FALSE, fun = NULL, ...) {
  # TODO: get more meaningful warnings and errors by replacing fun by actual
  # name of the function
  if (is.null(fun)) {# Default function depends on the chosen method
    if (method %in% c("maximum", "binary", "minkowski")) {
      fun <- stats::dist
    } else {
      fun <- vegan::vegdist # Note: euclidean/manhattan/canberra in both, but
      # we prioritize vegdist, and canberra is not calculated the same in dist!
    }
  }
  # We accept only formulas with right-hand side => length must be two
  if (length(formula) == 3)
    stop("The formula cannot have a left-hand term")

  # With matrices, we don't use rownames.col: rownames are already correctly set
  if (!is.matrix(data)) {# row names may be in a column (usual for tibbles)
    data <- as.data.frame(data)
    if (rownames.col %in% names(data)) {
      rownames(data) <- data[[rownames.col]]
      data[[rownames.col]] <- NULL
    } else {# rownames.col is NOT used
      rownames.col <- NULL
    }
    if (as.character(formula[2] != ".")) {
      # Subset the columns
      data <- model.frame(formula, data = data, subset = subset)
    } else if (!is.null(subset)) {
      data <- data[subset, ]
    }
  } else {# A matrix
    rownames.col <- NULL
    if (as.character(formula[2] != ".")) {
      # Subset the columns (and possibly the rows)
      if (is.null(subset)) {
        data <- data[, all.vars(formula)]
      } else {
        data <- data[subset, all.vars(formula)]
      }
    }
  }

  if (isTRUE(transpose))
    data <- t(data)

  # Arguments method =/metric = and stand = not always there
  if (!is.null(as.list(args(fun))$metric)) {# metric = instead of method =
    dst <- fun(data, metric = method, stand = scale, ...)
  } else if (isTRUE(scale)) {
    if (is.null(as.list(args(fun))$stand)) {# fun has no stand = argument
      data <- scale(data)
      dst <- fun(data, method = method, ...)
    } else {# We don't standardise ourself because there may be also qualitative
      # or binary data (like for cluster::daisy, for instance)
      dst <- fun(data, method = method, stand = scale, ...)
    }
  } else {# Just method = and scale = FALSE
    dst <- fun(data, method = method, ...)
  }
  attr(dst, "call") <- match.call()
  # Depending if it is a dist or dissimilarity object, the method is stored in
  # method or in Metric, but we use metric in our own version to avoid a clash
  # with the method item in cluster()/hclust() further on (hclust change it
  # into dist.method, but it is better to have the right name right now)
  attr(dst, "metric") <- method
  # dist or dissimilarity object use Labels, but we use labels everywhere else
  # including in cluster()/hclust()
  # So, we make sure labels is present (in hclust, it is labels anyway!)
  attr(dst, "labels") <- rownames(data)
  # Default values for Diag and Upper set to FALSE
  if (is.null(attr(dst, "Diag"))) attr(dst, "Diag") <- FALSE
  if (is.null(attr(dst, "Upper"))) attr(dst, "Upper") <- FALSE
  # Keep info about how raw data were transformed
  attr(dst, "rownames.col") <- rownames.col
  attr(dst, "transpose") <- transpose
  attr(dst, "scale") <- scale
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

as.dissimilarity <- function(x, ...)
  UseMethod("as.dissimilarity")

as_dissimilarity <- as.dissimilarity # Synonym

as.dissimilarity.matrix <- function(x, ...) {
  dst <- as.dist(x, ...)
  attr(dst, "call") <- match.call()
  attr(dst, "metric") <- attr(dst, "method") # Make sur metric is used
  class(dst) <- unique(c("dissimilarity", class(dst)))
  dst
}

# We want to print only the first few rows and columns
print.dissimilarity <- function(x, digits.d = 3L, rownames.lab = "labels",
  ...) {
  mat <- as.matrix(x)
  mat <- format(round(mat, digits.d))
  diag(mat) <- ""
  mat[upper.tri(mat)] <- ""
  class(mat) <- c("dst", "matrix")
  tbl <- tibble::as_tibble(mat)
  #tbl <- tibble::add_column(tbl, {{rownames.lab}} = rownames(mat), .before = 1)
  # I prefer this
  tbl <- dplyr::bind_cols(
    as_tibble_col(rownames(mat), column_name = rownames.lab), tbl)
  tbl <- tbl[, -ncol(tbl)]
  more_info <- ""
  if (isTRUE(attr(x, "scale"))) {
    if (isTRUE(attr(x, "transpose"))) {
      more_info <- " (transposed then scaled data)"
    } else {# Only scaled
      more_info <- " (scaled data)"
    }
  } else {
    if (isTRUE(attr(x, "transpose")))
      more_info <- " (transposed data)"
  }
  cat("Dissimilarity matrix with metric: ", attr(x, "metric"),
    more_info, "\n", sep = "")
  print(tbl)
  invisible(x)
}

labels.dissimilarity <- function(object, ...) {
  labs <- object$labels
  if (is.null(labs)) object$Labels
}

nobs.dissimilarity <- function(object, ...)
  attr(object, "Size")

# TODO: `[` by first transforming into a matrix with as.matrix()

autoplot.dissimilarity <- function(object, order = TRUE, show.labels = TRUE,
  lab.size = NULL, gradient = list(low = "red", mid = "white", high = "blue"),
  ...) {
  factoextra::fviz_dist(object, order = order, show_labels = show.labels,
    lab_size = lab.size, gradient = gradient)
}

chart.dissimilarity <- function(data, ...,
  type = NULL, env = parent.frame())
  autoplot(data, type = type, ...)

# cluster object (inheriting from hclust)
cluster <- function(x, ...)
  UseMethod("cluster")

cluster.default <- function(x, ...)
  stop("No method for object of class ", class(x)[1])

# Cluster uses hclust() by default, ... but it looks first for a faster
# implementation in either {fastcluster} or {flashClust} before falling back
# to the {stats} version.
# The functions cluster::agnes() and cluster::diana() should be compatible too,
# as well as any function that returns an object convertible into hclust
# by as.hclust() (but not tested yet)
# Also, a version where the raw data are provided and the disimilarity matrix
# is internally calculated should be also provided (see cluster::agnes)
# See also {ape} for phylogenetic trees methods
cluster.dist <- function(x, method = "complete", fun = NULL, ...) {
  if (is.null(fun)) {
    # We try fastcluster, then flashClust, then stats
    fun <- try(fastcluster::hclust, silent = TRUE)
    if (inherits(fun, "try-error"))
      fun <- try(flashClust::hclust, silent = TRUE)
    if (inherits(fun, "try-error"))
      fun <- try(stats::hclust, silent = TRUE)
  }
  clst <- fun(x, method = method, ...)
  clst <- as.hclust(clst)
  clst$call <- match.call()
  # hclust has to give a different name to the distance metric: dist.method
  # but we use metric. Again, keep both for maximum compatibility
  clst$metric <- clst$dist.method
  # If the original data were scaled or transposed, get the info also
  clst$rownames.col <- attr(x, "rownames.col")
  clst$scale <- attr(x, "scale")
  clst$transpose <- attr(x, "transpose")
  class(clst) <- unique(c("cluster", class(clst)))
  clst
}

# A couple of useful methods for our cluster object
# str() method is gathered from a dendrogram object
str.cluster <- function(object, max.level = NA, digits.d = 3L, ...)
  str(as.dendrogram(object), max.level = max.level, digits.d = digits.d, ...)

labels.cluster <- function(object, ...)
  object$labels

nobs.cluster <- function(object, ...)
  length(object$order)

# Other methods by first transforming into dendrogram: rev, reorder, order, [[

# cutree() is an explicit name, but it does not follow the rule of using
# known methods... and here, it really something that predict() is made for,
# except it cannot handle newdata =, but that argument is not in its definition
predict.cluster <- function(object, k = NULL, h = NULL, ...)
  cutree(object, k = k, h = h)

# There is no broom::glance() or broom::tidy() yet (what to put in it?),
# but broom:augment() should be nice = add the clusters as .fitted in the tibble
library(broom)
augment.cluster <- function(x, data, k = NULL, h = NULL, ...) {
  # Should we transpose the data (note: this is against augment() rules, but...)
  if (isTRUE(x$transpose)) {
    # We first have to make sure rownames are correct before the transposition
    if (!is.matrix(data) && !is.null(data[[x$rownames.col]])) {
      rownames(data) <- data[[x$rownames.col]]
      data[[x$rownames.col]] <- NULL
    }
    data <- t(data)
    msg <- "transposed data"
  } else {
    msg <- "data"
  }
  data <- as_tibble(data)

  # Get clusters
  clst <- predict(x, k = k, h = h, ...)
  if (nrow(data) != length(clst)) {
    stop("Different number of items in ", msg, " (",nrow(data) ,
      ") and in the clusters (", length(clst), ")")
  }
  tibble::add_column(data, .fitted = clst)
}

# Instead of the default plot.hclust(), we prefer the plot.dendrogram() version
# that allows for more and better variations of the dendrogram (horizontal or
# circular), see http://www.sthda.com/english/wiki
# /beautiful-dendrogram-visualizations-in-r-5-must-known-methods
# -unsupervised-machine-learning
plot.cluster <- function(x, y, labels = TRUE, hang = -1, check = TRUE,
  type = "vertical", lab = "Height", ...) {
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))
  # type == "circular" is special because we need to transform as ape::phylo
  if (type == "circular") {
    if (!missing(hang))
      warning("'hang' is not used with a circular dendrogram")
    phylo <- ape::as.phylo(x)
    plot(phylo, type = "fan", font = 1, show.tip.label = labels, ...)
  } else {# Use plot.dendrogram() instead
    # We first convert into dendrogram objet, then we plot it
    # (better that plot.hclust())
    if (isTRUE(labels)) leaflab <- "perpendicular" else leaflab <- "none"
    dendro <- as.dendrogram(x, hang = hang, check = check)
    if (type == "horizontal") {
      plot(dendro, horiz = TRUE, leaflab = leaflab, xlab = lab, ...)
    } else {
      plot(dendro, horiz = FALSE, leaflab = leaflab, ylab = lab, ...)
    }
  }
}

# This is to draw circles in a plot (where to cut in a circular dendrogram)
# TODO: should be nice to do similar function for other symbols too in SciViews
circle <- function(x = 0, y = 0, d = 1, col = 0, lwd = 1, lty = 1, ...)
  symbols(x = x, y = y, circles = d / 2, fg = col, lwd = lwd, lty = lty,
    inches = FALSE, add = TRUE, ...)

# TODO: make sure the dendrogram is correct with different ggplot themes
autoplot.cluster <- function(object, labels = TRUE, type = "vertical",
  circ.text.size = 3, theme = theme_sciviews(), xlab = "", ylab = "Height", ...) {
  if (is.null(type))
    type <- "vertical"
  type <- match.arg(type[1], c("vertical", "horizontal", "circular"))

  # Create the dendrogram
  ddata <- ggdendro::dendro_data(object, type = "rectangle")
  dendro <- ggplot(ggdendro::segment(ddata)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme + xlab(xlab) + ylab(ylab)

  if (type == "circular") {
    if (isTRUE(labels)) {
      # Get labels (need one more to avoid last = first!)
      label_df <- tibble::tibble(labels = c(labels(object)[object$order], ""))
      xmax <- nobs(object) + 1
      label_df$id <- 1:xmax
      angle <-  360 * (label_df$id - 0.5) / xmax
      # Left or right?
      label_df$hjust <- ifelse(angle < 270 & angle > 90, 1, 0)
      # Angle for more readable text
      label_df$angle <- ifelse(angle < 270 & angle > 90, angle + 180, angle)
    }

    # Make the dendrogram circular
    dendro <- dendro +
      scale_x_reverse() +
      scale_y_reverse() +
      coord_polar(start = pi/2)
    if (isTRUE(labels))
      dendro <- dendro +
      geom_text(data = label_df,
        aes(x = id, y = -0.02, label = labels, hjust = hjust),
        size = circ.text.size, angle = label_df$angle, inherit.aes = FALSE)
    dendro <- dendro +
      theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank()) +
      ylab("")

  } else if (type == "vertical") {# Vertical dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
        labels = ddata$labels$label) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
      theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
    if (!isTRUE(labels))
      dendro <- dendro +
        theme(axis.text.x = element_blank())

  } else {# Horizontal dendrogram
    dendro <- dendro +
      scale_x_continuous(breaks = seq_along(ddata$labels$label),
        labels = ddata$labels$label, position = "top") +
      scale_y_reverse(expand = expansion(mult = c(0.05, 0))) +
      coord_flip() +
      theme(panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())
    if (!isTRUE(labels))
      dendro <- dendro +
        theme(axis.text.y = element_blank())
  }
  dendro
}

chart.cluster <- function(data, ...,
  type = NULL, env = parent.frame())
  autoplot(data, type = type, ...)

# To indicate where to cut in the dendrogram, one could use `geom_hline()`,
# but when the dendrogram is horizontal or circular, this is suprizing. So,
# I define geom_dendroline(h = ....)
geom_dendroline <- function(h, ...)
  geom_hline(yintercept = h, ...)
