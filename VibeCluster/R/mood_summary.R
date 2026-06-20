#' Simple k Selection using Within-Cluster Sum of Squares
#'
#' @description
#' This function helps you choose a good number of clusters (k) for k-means.
#' It runs k-means for different values of k and records how tightly
#' the data points are grouped within clusters.
#'
#' The idea:
#' - Smaller within-cluster sum of squares (WSS) = tighter clusters
#' - As k increases, WSS always decreases
#' - We look for a point where the decrease slows down (the "elbow")
#'
#' @param songs A data.frame containing the required feature columns.
#' @param k_range Integer vector of k values to try (default = 2:8).
#' @param nstart Number of random initialisations for k-means (default = 10).
#' @param seed Random seed for reproducibility (default = 2026).
#'
#' @return A data.frame with:
#' \describe{
#'   \item{k}{Number of clusters}
#'   \item{within_ss}{Total within-cluster sum of squares}
#' }
#'
#' @details
#' For each value of k:
#' 1. Run k-means clustering
#' 2. Compute total within-cluster sum of squares:
#'    Sum of squared distances between each point and its cluster center
#' 3. Store the result
#'
#' Then a plot is created:
#' - x-axis: k (number of clusters)
#' - y-axis: WSS
#'
#' Interpretation:
#' - Steep drop → adding clusters improves fit
#' - Flat region → adding clusters gives little improvement
#' - Choose k at the "bend" (elbow point)
#'
#' @examples
#' # Load dataset
#' songs <- load_songs()
#'
#' # Try k from 2 to 8
#' df <- mood_summary(songs, k_range = 2:8)
#'
#' # View results
#' print(df)
#'
#' # Choose k where curve bends
#'
#' # Custom example
#' df2 <- mood_summary(songs, k_range = 3:10, nstart = 20)
#'
#' @export
mood_summary <- function(songs, k_range = 2:8,
                         nstart = 10, seed = 2026) {
  
  # ---- Input validation ----
  if (!is.data.frame(songs)) {
    stop("`songs` must be a data.frame.")
  }
  
  # Check required feature columns exist
  missing_feats <- setdiff(.FEATURES, names(songs))
  if (length(missing_feats) > 0) {
    stop("Missing feature columns: ",
         paste(missing_feats, collapse = ", "))
  }
  
  # Ensure k values are valid integers >= 2
  k_range <- sort(unique(as.integer(k_range)))
  if (any(k_range < 2L)) {
    stop("k must be >= 2")
  }
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Convert feature columns to numeric matrix
  feat_mat <- as.matrix(songs[, .FEATURES])
  
  # Store WSS values
  wss_vec <- numeric(length(k_range))
  
  # ---- Run k-means for each k ----
  for (i in seq_along(k_range)) {
    
    k <- k_range[i]
    
    cat("Running k =", k, "\n")
    
    # Apply k-means clustering
    km <- stats::kmeans(feat_mat,
                        centers = k,
                        nstart = nstart)
    
    # Store total within-cluster sum of squares
    wss_vec[i] <- km$tot.withinss
  }
  
  # ---- Plot results ----
  graphics::plot(k_range, wss_vec,
                 type = "b", pch = 19,
                 xlab = "Number of clusters (k)",
                 ylab = "Within-cluster sum of squares",
                 main = "Choosing k (simple method)")
  
  # ---- Output ----
  out <- data.frame(
    k = k_range,
    within_ss = wss_vec
  )
  
  return(out)
}