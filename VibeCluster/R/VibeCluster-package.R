#' VibeCluster: Mood-Based Music Clustering and Recommendation
#'
#' @description
#' VibeCluster groups songs into mood clusters using k-means on audio features
#' (energy, valence, danceability, tempo, acousticness, loudness).
#' It also provides visualisation using PCA and song recommendations
#' using nearest neighbours.
#'
#' @section Methods Used:
#' \itemize{
#'   \item K-means clustering for grouping songs
#'   \item PCA for 2D visualisation (Mood Map)
#'   \item Nearest neighbour search for recommendations
#'   \item  Simple k Selection using Within-Cluster Sum of Squares
#' }
#'
#' @section Workflow:
#' \preformatted{
#' library(VibeCluster)
#'
#' songs <- load_songs()
#' mood_summary(songs, k_range = 2:8)
#'
#' result <- mood_cluster(songs, k = 6)
#' plot_mood_map(result)
#'
#' get_mood_palette(result)
#'
#' my_mood <- c(energy = 0.8, valence = 0.3, danceability = 0.6,
#'              tempo_norm = 0.75, acousticness = 0.1, loudness_norm = 0.85)
#'
#' recommend_songs(result, profile = my_mood, n = 8)
#' }
#'
#' @section Data:
#' The package includes a dataset of 364 songs with mood labels.
#' Use load_songs() to access it.
#'
#' @docType package
#' @name VibeCluster
#'
#' @importFrom grDevices adjustcolor chull
#' @importFrom graphics plot points text legend par title rect axis
#'   barplot segments polygon abline grid
#' @importFrom stats kmeans prcomp sd dist
#' @importFrom utils read.csv
"_PACKAGE"


# -------------------------------
# Internal constants
# -------------------------------

# Feature names used everywhere
.FEATURES <- c(
  "energy", "valence", "danceability",
  "tempo_norm", "acousticness", "loudness_norm"
)


# Mood archetypes (reference points)
.ARCHETYPES <- matrix(
  c(
    0.72, 0.85, 0.78, 0.60, 0.20, 0.65,  # Happy
    0.28, 0.18, 0.32, 0.35, 0.70, 0.30,  # Melancholic
    0.92, 0.65, 0.85, 0.90, 0.08, 0.90,  # Energetic
    0.22, 0.58, 0.38, 0.25, 0.82, 0.22,  # Calm
    0.88, 0.22, 0.55, 0.82, 0.10, 0.95,  # Angry
    0.38, 0.72, 0.52, 0.42, 0.60, 0.45   # Romantic
  ),
  nrow = 6,
  byrow = TRUE,
  dimnames = list(
    c("Happy", "Melancholic", "Energetic", "Calm", "Angry", "Romantic"),
    .FEATURES
  )
)


# Colour palette for moods
.MOOD_COLORS <- c(
  Happy       = "#F4C430",
  Melancholic = "#6A7BA2",
  Energetic   = "#E8472A",
  Calm        = "#5BAD92",
  Angry       = "#C0392B",
  Romantic    = "#D98EC0",
  Cluster1    = "#E67E22",
  Cluster2    = "#2980B9",
  Cluster3    = "#27AE60",
  Cluster4    = "#8E44AD",
  Cluster5    = "#E74C3C",
  Cluster6    = "#16A085",
  Cluster7    = "#F39C12",
  Cluster8    = "#2C3E50"
)


# -------------------------------
# Helper functions
# -------------------------------

# Assign closest mood label to a centroid
.label_centroid <- function(centroid) {
  dists <- apply(.ARCHETYPES, 1, function(x) {
    sqrt(sum((centroid - x)^2))
  })
  names(which.min(dists))
}


# Simple Euclidean distance
.euc <- function(a, b) {
  sqrt(sum((a - b)^2))
}