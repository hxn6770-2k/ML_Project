# K-Means Clustering and User Identification on Interest Data

## Project Overview

This project applies **K-Means clustering** to interest data collected from multiple participants, with the aim of identifying clusters of similar interest patterns. The clustering process is enhanced by using advanced techniques such as **K-Means++ initialization**, **the elbow method**, and **silhouette analysis**. The goal is to determine optimal clusters (`k`) and evaluate the performance, which can be used for user identification based on unique interest profiles.

### Key Features and Enhancements

- **K-Means++ Initialization**: Enhanced initialization technique to select cluster centers, improving accuracy and convergence speed.
- **Optimal Cluster Selection**: Implements the **elbow method** to determine the optimal number of clusters.
- **Cluster Evaluation**: Uses **silhouette analysis** to evaluate the quality of clusters, ensuring accurate grouping of participants.
- **User Identification**: Participants are assigned to clusters based on their interest data for potential user identification.
- **Visualization**: Visualizations of the clusters and their centers are generated for easier interpretation of the results.

## Project Structure

- **Clustering**: K-Means clustering is performed to group users based on their interest data.
- **Optimal Cluster Selection**: The elbow method is employed to determine the optimal number of clusters (`k`).
- **Cluster Evaluation**: The clustering performance is evaluated using silhouette scores and the within-cluster sum of squares (WCSS).
- **User Assignment**: Users are assigned to clusters based on their interest data.
- **Visualization**: Cluster centers and participant distributions are visualized to interpret the results.

## Files

- `data/`: Contains CSV files with participants' interest data.
- `Vivian Nguyen_User Indentification From Walking Activity.R`: Main script for data processing, K-Means clustering, and result visualization.
- `README.md`: Project documentation, outlining the steps, structure, and usage of the code.

## Requirements

To run the project, ensure that the following software and packages are installed:

- **R version 4.0 or higher**
- **Required R Libraries**:
  - `dplyr`: Data manipulation
  - `cluster`: Clustering algorithms and evaluation metrics
  - `ggplot2`: Data visualization
  - `factoextra`: Visualization tools for clustering results

Install the required libraries in R with the following command:

```r
install.packages(c("dplyr", "cluster", "ggplot2", "factoextra"))
