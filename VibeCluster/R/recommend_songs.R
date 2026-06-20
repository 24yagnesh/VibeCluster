#' Recommend Songs for a Custom Mood Profile (kNN)
#'
#' Given a user-defined audio-feature vector (a "mood profile"), this
#' function uses \strong{k-Nearest Neighbours} (kNN) to find the \eqn{n}
#' most similar songs in the clustered dataset. Similarity is measured by
#' Euclidean distance in the 6-D feature space.
#'
#' @param result A \code{mood_result} object from \code{\link{mood_cluster}}.
#' @param profile A named numeric vector with the six audio features
#'   (\code{energy}, \code{valence}, \code{danceability}, \code{tempo_norm},
#'   \code{acousticness}, \code{loudness_norm}), each in \eqn{[0, 1]}.
#'   Names must match the feature column names exactly. You may omit any
#'   feature (it will be set to the dataset mean for that feature).
#' @param n Integer. Number of songs to recommend. Default \code{10}.
#' @param from_mood Character or \code{NULL}. If provided, restrict
#'   recommendations to songs belonging to this mood cluster (e.g.
#'   \code{"Calm"}). Default \code{NULL} searches all clusters.
#' @param show_plot Logical. If \code{TRUE} (default), a small Mood Map
#'   is drawn with the query point highlighted and the top-\eqn{n}
#'   recommendations marked.
#'
#' @return A \code{data.frame} with \eqn{n} rows and columns:
#'   \describe{
#'     \item{\code{rank}}{Recommendation rank (1 = closest).}
#'     \item{\code{title}}{Song title (if present in the dataset).}
#'     \item{\code{artist}}{Artist name (if present).}
#'     \item{\code{mood}}{Mood cluster label assigned by k-means.}
#'     \item{\code{distance}}{Euclidean distance from the query profile
#'       to the song in feature space.}
#'     \item{\code{energy}, \code{valence}, \ldots}{Feature values of
#'       each recommended song.}
#'   }
#'
#' @details
#' \strong{kNN is not trained separately:} it simply computes the Euclidean
#' distance from the query vector to every song in the dataset and returns
#' the \eqn{n} smallest distances. This is the exhaustive (brute-force) kNN
#' approach - efficient for datasets of a few thousand songs.
#'
#' \strong{Mood of the query:} After finding the nearest neighbours, the
#' function also reports which mood cluster the query profile would be
#' assigned to (the cluster whose centroid is closest to the profile).
#'
#' @examples
#' songs  <- load_songs()
#' result <- mood_cluster(songs, k = 6)
#'
#' # Define a high-energy, low-valence (angry/intense) profile
#' my_profile <- c(energy = 0.90, valence = 0.15,
#'                 danceability = 0.55, tempo_norm = 0.85,
#'                 acousticness = 0.05, loudness_norm = 0.92)
#' recs <- recommend_songs(result, profile = my_profile, n = 8)
#' print(recs)
#'
#' # Calm, acoustic profile
#' calm_profile <- c(energy = 0.18, valence = 0.60,
#'                   danceability = 0.35, tempo_norm = 0.20,
#'                   acousticness = 0.88, loudness_norm = 0.18)
#' recommend_songs(result, profile = calm_profile, n = 6, from_mood = "Calm")
#'
#' @seealso \code{\link{mood_cluster}}, \code{\link{plot_mood_map}}
#' @export
recommend_songs <- function(result, profile, n = 10,
                            from_mood = NULL, show_plot = TRUE) {
  
  if (!inherits(result, "mood_result")) {
    stop("`result` must be a `mood_result` from mood_cluster().")
  }
  
  n <- as.integer(n)
  if (is.na(n) || n < 1L) stop("`n` must be a positive integer.")
  
  # ---- Build query vector ----
  col_means <- colMeans(result$feature_matrix)
  query <- col_means
  names(query) <- .FEATURES
  
  if (!is.numeric(profile) || is.null(names(profile))) {
    stop("`profile` must be a named numeric vector with feature names as names.\n",
         "Required features: ", paste(.FEATURES, collapse = ", "))
  }
  unknown <- setdiff(names(profile), .FEATURES)
  if (length(unknown) > 0) {
    warning("Unknown feature(s) in `profile` (ignored): ",
            paste(unknown, collapse = ", "))
  }
  valid <- intersect(names(profile), .FEATURES)
  query[valid] <- profile[valid]
  
  # ---- Filter by mood if requested ----
  songs_pool <- result$songs
  if (!is.null(from_mood)) {
    matched <- grep(from_mood, songs_pool$mood, ignore.case = TRUE, value = TRUE)
    if (length(matched) == 0) {
      warning("`from_mood` '", from_mood, "' not found. ",
              "Available moods: ", paste(unique(songs_pool$mood), collapse = ", "),
              ". Searching all clusters.")
    } else {
      songs_pool <- songs_pool[songs_pool$mood %in% matched, , drop = FALSE]
    }
  }
  
  # ---- Compute Euclidean distances ----
  feat_sub <- as.matrix(songs_pool[, .FEATURES])
  dists    <- sqrt(rowSums(sweep(feat_sub, 2, query)^2))
  
  top_idx  <- order(dists)[seq_len(min(n, nrow(songs_pool)))]
  recs     <- songs_pool[top_idx, , drop = FALSE]
  recs$distance <- round(dists[top_idx], 4)
  recs$rank     <- seq_along(top_idx)
  
  # ---- Predicted mood of query ----
  ctr_dists   <- apply(result$centers, 1, function(c) .euc(query, c))
  query_mood  <- result$mood_labels[which.min(ctr_dists)]
  
  # ---- Format output ----
  keep_cols <- c("rank", intersect(c("title", "artist"), names(recs)),
                 "mood", "distance", .FEATURES)
  out <- recs[, keep_cols, drop = FALSE]
  out <- as.data.frame(out)
  rownames(out) <- NULL
  
  cat("=== VibeCluster Recommendations ===\n")
  cat("  Query mood (nearest centroid):", query_mood, "\n")
  cat("  Searched                     :",
      if (is.null(from_mood)) "All clusters" else from_mood, "\n")
  cat("  Top", nrow(out), "recommendations:\n\n")
  print_cols <- intersect(c("rank", "title", "artist", "mood", "distance"), names(out))
  print(as.data.frame(out[, print_cols]), row.names = FALSE)
  
  # ---- Optional plot ----
  if (show_plot) {
    pca     <- result$pca_obj
    scores  <- pca$x[, 1:2, drop = FALSE]
    var_exp <- pca$sdev^2 / sum(pca$sdev^2)
    
    q_proj <- matrix(query - pca$center, nrow = 1) %*% pca$rotation[, 1:2]
    
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mar = c(5, 4.5, 4, 2))
    
    xlim <- range(c(scores[, 1], q_proj[1])) * 1.1
    ylim <- range(c(scores[, 2], q_proj[2])) * 1.1
    
    xlab <- paste0("PC 1  (", round(var_exp[1]*100,1), "%)")
    ylab <- paste0("PC 2  (", round(var_exp[2]*100,1), "%)")
    
    graphics::plot(scores[, 1], scores[, 2],
                   col  = grDevices::adjustcolor("grey70", 0.40),
                   pch  = 19, cex = 0.55,
                   xlim = xlim, ylim = ylim,
                   xlab = xlab, ylab = ylab,
                   main = paste0("Mood Map - Top ", nrow(out),
                                 " Recommendations  [", query_mood, "]"),
                   bty  = "l", las = 1)
    
    graphics::grid(col = "grey93", lty = 1)
    graphics::abline(h = 0, v = 0, col = "grey80", lty = 2)
    
    rec_global_idx <- match(rownames(recs), rownames(result$songs))
    if (any(!is.na(rec_global_idx))) {
      rec_scores <- scores[rec_global_idx[!is.na(rec_global_idx)], , drop=FALSE]
      graphics::points(rec_scores[, 1], rec_scores[, 2],
                       pch = 21, bg = "#E8472A", col = "white",
                       cex = 1.6, lwd = 1.2)
    }
    
    graphics::points(q_proj[1], q_proj[2],
                     pch = 8, col = "#2c3e50", cex = 3, lwd = 2.5)
  }
  
  invisible(out)
}