############################################################
# runme.R
# Demonstration of VibeCluster Package
# Author: Yagnesh
# Usage: source("runme.R")
############################################################

start_time <- Sys.time()

# ── 1. Load Package ────────────────────────────────────────
library(VibeCluster)

# ── 2. Load Dataset ────────────────────────────────────────
cat("Loading sample songs dataset...\n")
songs <- load_songs()
print(head(songs))
cat("Dataset dimensions:", nrow(songs), "songs x", ncol(songs), "columns\n")

# ── 3. Choose Optimal k ────────────────────────────────────
cat("\nEvaluating optimal k using elbow method (WSS)...\n")
df <- mood_summary(songs, k_range = 2:8)
print(df)
cat("\nSelected k = 6 based on elbow in WSS plot\n")
k_opt <- 6

# ── 4. Cluster Songs ───────────────────────────────────────
cat("\nClustering", nrow(songs), "songs into", k_opt, "mood groups...\n")
result <- mood_cluster(songs, k = k_opt)
print(result)

# ── 5. Mood Distribution ───────────────────────────────────
cat("\nMood distribution (counts)...\n")
get_mood_palette(result)

cat("\nMood distribution (proportions)...\n")
get_mood_palette(result, type = "proportion")

cat("\nBar chart only (no feature heatmap)...\n")
get_mood_palette(result, show_features = FALSE)

# ── 6. Mood Map (PCA) ──────────────────────────────────────
cat("\nPlotting PCA Mood Map...\n")
plot_mood_map(result)

cat("\nMood Map without convex hulls...\n")
plot_mood_map(result, show_hulls = FALSE)

# NOTE: label_songs = TRUE is very cluttered for 364 songs
# Only enable for small custom datasets
# plot_mood_map(result, label_songs = TRUE)

# ── 7. Recommendations ─────────────────────────────────────
cat("\nGenerating personalised recommendations...\n")

# High energy / intense profile
intense_profile <- c(
  energy        = 0.90,
  valence       = 0.15,
  danceability  = 0.55,
  tempo_norm    = 0.85,
  acousticness  = 0.05,
  loudness_norm = 0.92
)
cat("\nHigh-energy recommendations:\n")
recs1 <- recommend_songs(result, profile = intense_profile, n = 8)
print(recs1)

# Calm / acoustic profile
calm_profile <- c(
  energy        = 0.18,
  valence       = 0.60,
  danceability  = 0.35,
  tempo_norm    = 0.20,
  acousticness  = 0.88,
  loudness_norm = 0.18
)
cat("\nCalm mood recommendations:\n")
recs2 <- recommend_songs(result, profile = calm_profile, n = 6,
                         from_mood = "Calm")
print(recs2)

# ── 8. Reproducibility Check ───────────────────────────────
cat("\nChecking reproducibility with fixed seed...\n")
res1 <- mood_cluster(songs, k = k_opt, seed = 123)
res2 <- mood_cluster(songs, k = k_opt, seed = 123)
cat("Results identical:", identical(res1$cluster_sizes,
                                    res2$cluster_sizes), "\n")

# ── 9. Custom Dataset (not executed) ───────────────────────
cat("\nTo use your own dataset, uncomment below:\n")
# my_songs  <- load_songs("path/to/your_file.csv")
# my_result <- mood_cluster(my_songs, k = k_opt)
# plot_mood_map(my_result)

# ── Done ───────────────────────────────────────────────────
cat("\nTotal execution time:",
    round(difftime(Sys.time(), start_time, units = "secs"), 2),
    "seconds\n")
cat("Demo completed successfully!\n")
############################################################
