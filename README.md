# VibeCluster 🎵

An R package for mood-based music clustering, visualization, and personalized song recommendation.

## Overview

VibeCluster helps users explore and organize music collections based on audio characteristics such as energy, valence, danceability, tempo, acousticness, and loudness.

The package automatically groups songs into mood clusters using K-Means clustering, visualizes mood spaces through Principal Component Analysis (PCA), and generates personalized song recommendations based on user-defined listening preferences.

## Features

* Mood-based song clustering using K-Means
* Automatic mood labeling and cluster summaries
* Elbow-method support for selecting the optimal number of clusters
* PCA-based mood map visualization
* Personalized recommendation engine
* Reproducible clustering through seed control
* Support for custom song datasets

## Installation

Install from a local source package:

```r
install.packages("VibeCluster_1.0.0.tar.gz",
                 repos = NULL,
                 type = "source")
```

Load the package:

```r
library(VibeCluster)
```

## Dataset

The package includes a sample dataset containing song-level audio features such as:

* Energy
* Valence
* Danceability
* Tempo
* Acousticness
* Loudness

Load the dataset:

```r
songs <- load_songs()
head(songs)
```

## Selecting the Optimal Number of Clusters

Use the elbow method to evaluate within-cluster sum of squares (WSS):

```r
df <- mood_summary(songs, k_range = 2:8)
print(df)
```

Example:

```r
k_opt <- 6
```

![Elbow Plot](images/elbow_plot.png)

## Mood Clustering

Cluster songs into mood groups:

```r
result <- mood_cluster(songs, k = 6)
```

View cluster information:

```r
print(result)
```

## Mood Distribution

Display mood counts:

```r
get_mood_palette(result)
```

Display proportions:

```r
get_mood_palette(result,
                 type = "proportion")
```

![Mood Distribution](images/mood_distribution.png)

## Mood Map Visualization

Generate a PCA-based mood map:

```r
plot_mood_map(result)
```

Disable convex hulls:

```r
plot_mood_map(result,
              show_hulls = FALSE)
```

![Mood Map](images/mood_map.png)

## Personalized Recommendations

Unlike mood-label-based recommenders, VibeCluster lets you define your own emotional fingerprint — a precise weight (0–1) across six independent audio dimensions — instead of picking from a fixed list of moods. The engine then finds songs that match your fingerprint using nearest-neighbour similarity, not rigid genre rules, so it can express in-between or unconventional moods (e.g. high energy + low valence for "aggressive," or low energy + high valence for "content") that a single mood label never could.

### High-Energy Profile

```r
profile <- c(
  energy        = 0.90,
  valence       = 0.15,
  danceability  = 0.55,
  tempo_norm    = 0.85,
  acousticness  = 0.05,
  loudness_norm = 0.92
)

recommend_songs(result,
                profile = profile,
                n = 8)
```

### Calm Acoustic Profile

```r
profile <- c(
  energy        = 0.18,
  valence       = 0.60,
  danceability  = 0.35,
  tempo_norm    = 0.20,
  acousticness  = 0.88,
  loudness_norm = 0.18
)

recommend_songs(result,
                profile = profile,
                n = 6,
                from_mood = "Calm")
```

## Reproducibility

The package supports deterministic clustering through random seed control.

```r
res1 <- mood_cluster(songs,
                     k = 6,
                     seed = 123)

res2 <- mood_cluster(songs,
                     k = 6,
                     seed = 123)

identical(res1$cluster_sizes,
          res2$cluster_sizes)
```

## Project Structure

```
VibeCluster/
├── R/
├── man/
├── data/
├── vignettes/
├── tests/
├── images/
├── runme.R
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## Methods Used

* K-Means Clustering
* Principal Component Analysis (PCA)
* Euclidean Distance Similarity
* k-Nearest Neighbour Recommendation Strategy
* Elbow Method for Model Selection

## Example Workflow

```r
library(VibeCluster)

songs <- load_songs()

mood_summary(songs)

result <- mood_cluster(songs, k = 6)

plot_mood_map(result)

recommend_songs(result,
                profile = my_profile,
                n = 10)
```

## Author

Yagnesh Bonnada
B.Tech Undergraduate
Indian Institute of Technology Kanpur
