#' Plot Mood Map (PCA Scatter)
#'
#' @description
#' Creates a 2D scatter plot of songs using Principal Component Analysis (PCA).
#' Each song is shown as a point, coloured by its assigned mood cluster.
#' Cluster centres (centroids) are highlighted and optionally labelled.
#'
#' @details
#' The function uses PCA to reduce the 6 audio features into 2 dimensions:
#' PC1 and PC2. This allows visualisation of high-dimensional data.
#'
#' The plot includes:
#' - Individual songs (points)
#' - Cluster centroids (larger markers)
#' - Optional convex hulls around clusters
#' - Optional labels and legend
#'
#' @param result A `mood_result` object returned by `mood_cluster()`.
#' @param show_songs Logical. If TRUE, plots individual song points.
#' @param show_labels Logical. If TRUE, shows labels for cluster centres.
#' @param show_hulls Logical. If TRUE, draws boundaries around clusters.
#' @param show_legend Logical. If TRUE, displays legend.
#' @param label_songs Logical. If TRUE, prints song titles (can be cluttered).
#' @param title Character. Title of the plot.
#' @param alpha_songs Numeric. Transparency level for song points (0 to 1).
#'
#' @return Invisibly returns a data.frame with:
#' \describe{
#'   \item{PC1}{First principal component}
#'   \item{PC2}{Second principal component}
#'   \item{mood}{Assigned mood cluster}
#'   \item{title}{Song title (if available)}
#' }
#'
#' @examples
#' # Load data
#' songs <- load_songs()
#'
#' # Cluster songs
#' result <- mood_cluster(songs, k = 6)
#'
#' # Plot mood map
#' plot_mood_map(result)
#'
#' # Without hulls
#' plot_mood_map(result, show_hulls = FALSE)
#'
#' # Show song labels (only for small datasets)
#' plot_mood_map(result, label_songs = TRUE)
#'
#' @export
plot_mood_map <- function(result,
                          show_songs   = TRUE,
                          show_labels  = TRUE,
                          show_hulls   = TRUE,
                          show_legend  = TRUE,
                          label_songs  = FALSE,
                          title        = "Mood Map",
                          alpha_songs  = 0.5) {
  
  if (!inherits(result, "mood_result")) {
    stop("`result` must be from mood_cluster().")
  }
  
  pca     <- result$pca_obj
  songs   <- result$songs
  centers <- result$centers
  labels  <- result$mood_labels
  colors  <- result$mood_colors
  cluster <- songs$cluster
  k       <- result$k
  
  # ---- PCA projection ----
  scores   <- pca$x[, 1:2]
  ctr_proj <- sweep(centers, 2, pca$center, "-") %*% pca$rotation[, 1:2]
  
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  xlab <- paste0("PC1 (", round(var_exp[1]*100, 1), "%)")
  ylab <- paste0("PC2 (", round(var_exp[2]*100, 1), "%)")
  
  # ---- Add padding to axes ----
  xlim <- range(scores[,1]) * 1.1
  ylim <- range(scores[,2]) * 1.1
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  par(mar = c(5, 5, 4, 2))
  
  plot(NULL,
       xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = ylab,
       main = title, bty = "l")
  
  grid(col = "grey90")
  abline(h = 0, v = 0, lty = 2, col = "grey75")
  
  # ---- Hulls (lighter + cleaner) ----
  if (show_hulls) {
    for (i in seq_len(k)) {
      idx <- which(cluster == i)
      if (length(idx) < 3) next
      
      hull <- chull(scores[idx, ])
      hull <- c(hull, hull[1])
      
      col_i <- colors[i]
      
      polygon(scores[idx, ][hull, ],
              col    = adjustcolor(col_i, 0.12),
              border = adjustcolor(col_i, 0.6),
              lwd    = 1)
    }
  }
  
  # ---- Song points (smaller + softer) ----
  if (show_songs) {
    for (i in seq_len(k)) {
      idx <- which(cluster == i)
      
      points(scores[idx, ],
             col = adjustcolor(colors[i], alpha_songs),
             pch = 16, cex = 0.6)
    }
  }
  
  # ---- Centroids (clear focus) ----
  for (i in seq_len(k)) {
    points(ctr_proj[i, 1], ctr_proj[i, 2],
           pch = 23, bg = colors[i],
           col = "white", cex = 2.5, lwd = 1.5)
    
    # Better label placement (slightly offset)
    if (show_labels) {
      text(ctr_proj[i, 1], ctr_proj[i, 2],
           labels = labels[i],
           pos = 4, offset = 0.6,
           cex = 0.85, font = 2)
    }
  }
  
  # ---- Optional song labels (keep minimal) ----
  if (label_songs && "title" %in% names(songs)) {
    text(scores,
         labels = songs$title,
         cex = 0.3, col = "grey40", pos = 3)
  }
  
  # ---- Legend (moved away from center) ----
  if (show_legend) {
    legend("top",
           legend = labels,
           col = colors,
           pch = 19,
           pt.cex = 1,
           cex = 0.8,
           bty = "n")
  }
  
  # ---- Output ----
  out <- data.frame(
    PC1 = scores[, 1],
    PC2 = scores[, 2],
    mood = songs$mood
  )
  
  if ("title" %in% names(songs)) {
    out$title <- songs$title
  }
  
  invisible(out)
}