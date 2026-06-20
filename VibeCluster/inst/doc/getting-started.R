## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7, fig.height = 5,
  out.width = "100%"
)

## ----load---------------------------------------------------------------------
library(VibeCluster)

songs <- load_songs()
print(songs)

## ----summary, fig.height=4.5--------------------------------------------------
df <- mood_summary(songs, k_range = 2:8)
df

## ----cluster------------------------------------------------------------------
result <- mood_cluster(songs, k = 6)
print(result)

## ----songs_preview------------------------------------------------------------
head(result$songs[, c("title", "artist", "mood", "energy", "valence")])

## ----moodmap, fig.height=5.5--------------------------------------------------
plot_mood_map(result)

## ----palette, fig.height=5----------------------------------------------------
pal <- get_mood_palette(result)
print(pal[, c("mood", "count", "proportion")])

## ----recs_angry---------------------------------------------------------------
# High-energy, dark mood — Angry/Energetic profile
my_profile <- c(energy        = 0.90,
                valence       = 0.15,
                danceability  = 0.55,
                tempo_norm    = 0.85,
                acousticness  = 0.05,
                loudness_norm = 0.92)

recs <- recommend_songs(result, profile = my_profile, n = 8)

## ----recs_calm, fig.height=5--------------------------------------------------
# Acoustic, gentle profile
calm_profile <- c(energy        = 0.18,
                  valence       = 0.65,
                  danceability  = 0.35,
                  tempo_norm    = 0.20,
                  acousticness  = 0.88,
                  loudness_norm = 0.18)

recs_calm <- recommend_songs(result, profile = calm_profile, n = 8,
                             from_mood = "Calm")

