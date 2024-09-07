# K-Means Clustering and User Identification on Interest Data

## Project Overview

This project implements K-Means clustering on interest data collected from multiple participants. The objective is to identify clusters of similar interest patterns, determine the optimal number of clusters (`k`), and evaluate clustering performance. This method can potentially be used for identifying users based on their unique interest profiles.

## Project Structure

- **Clustering**: K-Means clustering is used to group users based on their interest data.
- **Optimal `k` Determination**: The elbow method is employed to determine the optimal number of clusters (`k`).
- **User Identification**: After clustering, users are assigned to their nearest cluster for potential identification.
- **Evaluation**: The clustering is evaluated using silhouette scores and within-cluster sum of squares (WCSS).
- **Visualization**: Cluster centers and distributions are visualized for better interpretation of results.

## Files

- `data/`: Directory containing CSV files for each participant's interest data.
- `main.R`: Main R script that processes data, performs clustering, and visualizes results.
- `README.md`: Documentation for the project.

## Requirements

To run this project, ensure you have the following software installed:

- **R version 4.0 or higher**
- **Required R Libraries**:
  - `dplyr`
  - `cluster`
  - `ggplot2`
  - `factoextra`

You can install the necessary libraries using this command in R:

```r
install.packages(c("dplyr", "cluster", "ggplot2", "factoextra"))
