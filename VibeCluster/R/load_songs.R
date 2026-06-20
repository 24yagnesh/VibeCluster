#' Load Song Dataset
#'
#' Reads a CSV file of songs with audio features and prepares it for
#' clustering. If no path is provided, the built-in dataset is used.
#'
#' @param path Character. Path to CSV file. If NULL, built-in data is used.
#' @param scale Logical. If TRUE, features are standardised.
#'
#' @return A data.frame of class "mood_songs"
#'
#' @examples
#' # Load built-in dataset
#' songs <- load_songs()
#' head(songs)
#'
#' # Load your own dataset
#' \dontrun{
#' my_songs <- load_songs("path/to/your_file.csv")
#' }
#'
#' # Load and scale features
#' songs_scaled <- load_songs(scale = TRUE)
#'
#' @export
load_songs <- function(path = NULL, scale = FALSE) {
  
  # ---- Resolve file path ----
  if (is.null(path)) {
    path <- system.file("extdata", "mood_songs.csv",
                        package = "VibeCluster")
    if (!nzchar(path)) {
      stop("Built-in dataset not found. Please reinstall VibeCluster.")
    }
  } else {
    if (!file.exists(path)) {
      stop("File not found: ", path)
    }
  }
  
  # ---- Read data ----
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  
  # ---- Check required columns ----
  missing_cols <- setdiff(.FEATURES, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required feature columns:\n  ",
      paste(missing_cols, collapse = ", "), "\n",
      "Required: ", paste(.FEATURES, collapse = ", ")
    )
  }
  
  # ---- Optional scaling ----
  if (scale) {
    for (feat in .FEATURES) {
      mu  <- mean(df[[feat]], na.rm = TRUE)
      sig <- stats::sd(df[[feat]], na.rm = TRUE)
      if (sig > 0) {
        df[[feat]] <- (df[[feat]] - mu) / sig
      }
    }
  }
  
  # ---- Final formatting ----
  attr(df, "scaled") <- scale
  class(df) <- c("mood_songs", "data.frame")
  
  df
}


#' Built-in Song Dataset
#'
#' A dataset of 364 songs with six audio features and a reference mood label.
#' This is the same dataset returned by load_songs().
#'
#' @format A data.frame with 364 rows and feature columns:
#' energy, valence, danceability, tempo_norm, acousticness, loudness_norm
#'
#' @examples
#' # Access built-in dataset
#' songs <- load_songs()
#' dim(songs)
#'
#' @export
mood_songs <- NULL


#' Print Method for mood_songs
#'
#' Displays a quick summary of the dataset including number of songs,
#' features, mood distribution (if available), and scaling status.
#'
#' @param x A mood_songs object
#' @param ... Additional arguments (unused)
#'
#' @examples
#' songs <- load_songs()
#' print(songs)
#'
#' @export
print.mood_songs <- function(x, ...) {
  
  cat("=== VibeCluster Song Dataset ===\n")
  cat("Songs   :", nrow(x), "\n")
  cat("Features:", paste(.FEATURES, collapse = ", "), "\n")
  
  if ("mood_true" %in% names(x)) {
    tab <- sort(table(x$mood_true), decreasing = TRUE)
    cat("Moods   :", paste(names(tab), tab, sep = "=", collapse = "  "), "\n")
  }
  
  cat("Scaled  :", isTRUE(attr(x, "scaled")), "\n")
  
  invisible(x)
}