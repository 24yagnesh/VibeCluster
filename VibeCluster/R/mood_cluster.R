#' Cluster Songs into Mood Groups Using K-Means
#'
#' Applies k-means clustering to the six audio features and groups songs
#' into k clusters. Each cluster is optionally assigned a mood label based
#' on the closest predefined mood profile.
#'
#' @param songs A data.frame with the required feature columns.
#' @param k Number of clusters (2 to 8). Default 6.
#' @param nstart Number of random starts. Default 25.
#' @param iter.max Maximum iterations. Default 200.
#' @param seed Random seed. Default 2026.
#' @param label_clusters Assign mood labels if TRUE. Default TRUE.
#'
#' @return A list of class "mood_result".
#' @examples
#' ## Example 1: Basic usage
#' songs <- load_songs()
#' result <- mood_cluster(songs)
#' print(result)
#'
#' ## Example 2: Specify number of clusters
#' result_k4 <- mood_cluster(songs, k = 4)
#' print(result_k4)
#'
#' ## Example 3: Access clustered data
#' head(result$songs)
#'
#' ## Example 4: View cluster centers
#' result$centers
#'
#' ## Example 5: Disable mood labeling
#' result_raw <- mood_cluster(songs, label_clusters = FALSE)
#' print(result_raw)
#'
#' ## Example 6: Reproducibility with seed
#' result1 <- mood_cluster(songs, seed = 123)
#' result2 <- mood_cluster(songs, seed = 123)
#' identical(result1$cluster_sizes, result2$cluster_sizes)
#' @export
mood_cluster <- function(songs, k = 6, nstart = 25, iter.max = 200,
                         seed = 2026, label_clusters = TRUE) {
  
  # ---- Input checks ----
  if (!is.data.frame(songs)) {
    stop("`songs` must be a data.frame.")
  }
  
  missing_feats <- setdiff(.FEATURES, names(songs))
  if (length(missing_feats) > 0) {
    stop("Missing feature columns: ",
         paste(missing_feats, collapse = ", "),
         "\nUse load_songs() for a valid dataset.")
  }
  
  k <- as.integer(k)
  if (is.na(k) || k < 2L || k > 8L) {
    stop("`k` must be between 2 and 8.")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # ---- Feature matrix ----
  feat_mat <- as.matrix(songs[, .FEATURES])
  storage.mode(feat_mat) <- "double"
  
  # ---- K-means ----
  km <- stats::kmeans(
    feat_mat,
    centers = k,
    nstart = nstart,
    iter.max = iter.max
  )
  
  # ---- Label clusters ----
  mood_labels <- character(k)
  
  if (label_clusters) {
    used <- character(0)
    
    for (i in seq_len(k)) {
      lbl <- .label_centroid(km$centers[i, ])
      
      if (lbl %in% used) {
        lbl <- paste0(lbl, "_2")
      }
      
      mood_labels[i] <- lbl
      used <- c(used, lbl)
    }
    
  } else {
    mood_labels <- paste0("Cluster", seq_len(k))
  }
  
  names(mood_labels) <- seq_len(k)
  
  # ---- Colours ----
  default_pal <- c(
    "#F4C430", "#6A7BA2", "#E8472A", "#5BAD92",
    "#C0392B", "#D98EC0", "#E67E22", "#2980B9"
  )
  
  mood_colors <- character(k)
  
  for (i in seq_len(k)) {
    lbl <- mood_labels[i]
    base_lbl <- sub("_2$", "", lbl)
    
    if (base_lbl %in% names(.MOOD_COLORS)) {
      mood_colors[i] <- .MOOD_COLORS[[base_lbl]]
    } else {
      mood_colors[i] <- default_pal[(i - 1) %% length(default_pal) + 1]
    }
  }
  
  names(mood_colors) <- mood_labels
  
  # ---- Add cluster info to data ----
  songs_out <- songs
  songs_out$cluster <- km$cluster
  songs_out$mood    <- mood_labels[km$cluster]
  
  # ---- PCA (for plotting) ----
  pca_obj <- stats::prcomp(feat_mat, center = TRUE, scale. = FALSE)
  
  # ---- Cluster sizes ----
  csizes <- tabulate(km$cluster, nbins = k)
  names(csizes) <- mood_labels
  
  # ---- Output ----
  structure(
    list(
      songs          = songs_out,
      centers        = km$centers,
      mood_labels    = mood_labels,
      mood_colors    = mood_colors,
      cluster_sizes  = csizes,
      within_ss      = km$tot.withinss,
      kmeans_obj     = km,
      pca_obj        = pca_obj,
      k              = k,
      feature_matrix = feat_mat
    ),
    class = "mood_result"
  )
}


#' @export
print.mood_result <- function(x, ...) {
  
  cat("=== VibeCluster Result ===\n")
  cat("Songs    :", nrow(x$songs), "\n")
  cat("k        :", x$k, "\n")
  cat("Within SS:", round(x$within_ss, 4), "\n\n")
  
  df <- data.frame(
    Cluster = seq_len(x$k),
    Mood    = x$mood_labels,
    Songs   = x$cluster_sizes,
    Percent = paste0(round(x$cluster_sizes / nrow(x$songs) * 100, 1), "%"),
    stringsAsFactors = FALSE
  )
  
  rownames(df) <- NULL
  print(df)
  
  cat("\nFeatures:", paste(.FEATURES, collapse = ", "), "\n")
  
  invisible(x)
}