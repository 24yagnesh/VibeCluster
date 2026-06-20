#' Mood Distribution Plot
#'
#' @description
#' Displays how songs are distributed across mood clusters using a bar chart.
#' Optionally, it also shows a heatmap of average feature values for each mood.
#'
#' @details
#' The function provides two views:
#' \itemize{
#'   \item A bar plot showing number (or percentage) of songs in each mood.
#'   \item A feature heatmap showing average values of audio features
#'         (energy, valence, etc.) for each cluster.
#' }
#'
#' This helps in understanding:
#' - Which moods dominate the dataset
#' - How different moods vary across features
#'
#' @param result A `mood_result` object returned by `mood_cluster()`.
#' @param type Character. Either `"count"` (default) or `"proportion"`.
#' @param show_features Logical. If TRUE, displays feature heatmap.
#' @param title Character. Title of the bar plot.
#'
#' @return Invisibly returns a data.frame with:
#' \describe{
#'   \item{mood}{Mood label}
#'   \item{count}{Number of songs in each mood}
#'   \item{proportion}{Fraction of songs in each mood}
#'   \item{feature values}{Cluster-wise mean feature values}
#' }
#' @importFrom grDevices colorRampPalette
#' @examples
#' # Load data
#' songs <- load_songs()
#'
#' # Cluster songs
#' result <- mood_cluster(songs, k = 6)
#'
#' # Plot mood distribution (counts)
#' get_mood_palette(result)
#'
#' # Plot proportions instead of counts
#' get_mood_palette(result, type = "proportion")
#'
#' # Only bar plot (no heatmap)
#' get_mood_palette(result, show_features = FALSE)
#'
#' @export
get_mood_palette <- function(result,
                             type = c("count", "proportion"),
                             show_features = TRUE,
                             title = "Mood Distribution") {
  
  if (!inherits(result, "mood_result")) {
    stop("`result` must be from mood_cluster().")
  }
  
  type <- match.arg(type)
  
  labels <- result$mood_labels
  sizes  <- result$cluster_sizes
  
  values <- if (type == "proportion") {
    sizes / sum(sizes) * 100
  } else {
    sizes
  }
  
  ylab <- if (type == "proportion") "% of songs" else "Number of songs"
  colors <- result$mood_colors
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  if (show_features) {
    par(mfrow = c(1, 2), mar = c(6, 5, 4, 2))
  }
  
  ymax <- max(values) * 1.15
  
  bp <- barplot(values,
                names.arg = labels,
                col = colors,
                border = "white",
                ylab = ylab,
                main = title,
                las = 2,
                ylim = c(0, ymax))
  
  text(bp, values,
       labels = round(values, 1),
       pos = 3,
       offset = 0.8)
  
  #DOT PLOT
  if (show_features) {
    
    feat <- as.data.frame(result$centers)
    rownames(feat) <- labels
    
    feat_scaled <- scale(feat)
    
    n_row <- nrow(feat_scaled)
    n_col <- ncol(feat_scaled)
    
    # Empty plot
    plot(NULL,
         xlim = c(1, n_col),
         ylim = c(1, n_row),
         xaxt = "n", yaxt = "n",
         xlab = "Features",
         ylab = "Mood",
         main = "Feature Profile (Dot Plot)")
    
    axis(1, at = 1:n_col, labels = colnames(feat_scaled), las = 2)
    axis(2, at = 1:n_row, labels = rownames(feat_scaled), las = 2)
    
    # Grid
    abline(h = 1:n_row, col = "lightgray", lty = 2)
    abline(v = 1:n_col, col = "lightgray", lty = 2)
    
    # Color palette
    col_fun <- colorRampPalette(c("blue", "white", "red"))
    
    # Flatten values
    values <- as.vector(feat_scaled)
    
    # Normalize for size
    sizes <- 1 + 2 * (values - min(values)) / (max(values) - min(values))
    
    colors_dot <- col_fun(100)[cut(values, breaks = 100)]
    
    # Plot points
    points(
      x = rep(1:n_col, each = n_row),
      y = rep(1:n_row, times = n_col),
      pch = 19,
      cex = sizes,
      col = colors_dot
    )
    
  }
 
  centers_mat <- result$centers
  centers_mat <- as.matrix(centers_mat)
  centers_df <- data.frame(centers_mat, row.names = NULL)
  out <- data.frame(
    mood = as.character(labels),
    count = as.numeric(sizes),
    proportion = as.numeric(sizes / sum(sizes)),
    centers_df,
    row.names = NULL
  )
  invisible(out)
}