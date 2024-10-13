# --------------------------------------------------------
# Title: K-Means Clustering Analysis
# Your Name: Vivian Nguyen
# Course Title: CS-499 Computer Science Capstone
# Instructor: Brooke Goggin
# Submission Date: 09/28/2024
# --------------------------------------------------------

# Description:
# This script performs k-means clustering analysis on participant interest data.
# The script finds the optimal number of clusters (k) using the elbow method,
# evaluates the clustering quality, and visualizes the cluster distributions.

# --------------------------------------------------------

# Clear the environment
rm(list = ls())

# Load required libraries
library(dplyr)        # For data manipulation
library(cluster)      # For clustering algorithms
library(ggplot2)      # For data visualization
library(factoextra)   # For clustering visualization

# Function to read and preprocess data
read_and_preprocess <- function(filename) {
  # Read the CSV file
  data <- read.csv(filename, header = FALSE)
  
  # Perform Z-score standardization on the interests data frame
  interests <- as.data.frame(lapply(data[-1], scale))  # Remove the first column (time step) before scaling
  
  return(interests)
}

# Function to perform k-means clustering
perform_kmeans <- function(interests, k) {
  # Perform k-means clustering
  set.seed(2345) # Set seed for reproducibility
  
  kmeans_result <- kmeans(interests, centers = k, algorithm = "MacQueen", iter.max = 100, nstart = 25)
  
  return(kmeans_result)
}

# Function to calculate the perpendicular distance from a point to a line
perpendicular_distance <- function(x, y, x1, y1, x2, y2) {
  # Calculate the slope of the line
  m <- (y2 - y1) / (x2 - x1)
  
  # Calculate the equation of the line: y = mx + c
  c <- y1 - m * x1
  
  # Calculate the perpendicular distance
  distance <- abs(m * x - y + c) / sqrt(m^2 + 1)
  
  return(distance)
}

# Function to find optimal k using the elbow method
find_optimal_k <- function(interests, max_k = 20) {
  # Initialize a vector to store WCSS for each k
  wss <- numeric(max_k)
  
  # Calculate total within-cluster sum of squares for different values of k
  for (k in 1:max_k) {
    kmeans_result <- kmeans(interests, centers = k, nstart = 25) # Use nstart for better clustering
    wss[k] <- kmeans_result$tot.withinss  # Store WCSS for the current k
  }
  
  # Identify the 'elbow' point where WCSS decreases sharply
  elbow_point <- which.max(diff(diff(wss))) + 1 # Find the elbow point
  
  return(elbow_point)
}

# Function for user identification based on cluster assignment
user_identification <- function(interests, kmeans_result) {
  # Assign each user to the nearest cluster center
  cluster_centers <- kmeans_result$centers
  cluster_assignments <- apply(
    interests, 1, function(x) which.min(apply(cluster_centers, 1, function(y) sum((x - y)^2)))
  )
  
  return(cluster_assignments)
}

# Function to evaluate clustering quality using silhouette score and WCSS
evaluate_clustering <- function(kmeans_result, data, participant_index) {
  # Extract cluster assignments
  cluster_assignments <- kmeans_result$cluster
  
  # Calculate distances between data points
  dist_matrix <- dist(data)
  
  # Convert distance matrix to square matrix
  dist_matrix_square <- as.matrix(dist_matrix)
  
  # Initialize silhouette vector
  silhouette_values <- numeric(length(cluster_assignments))
  
  # Calculate silhouette values for each data point
  for (i in 1:length(cluster_assignments)) {
    # Get the cluster assignment of the current data point
    cluster <- cluster_assignments[i]
    
    # Calculate average distance to other points in the same cluster (a)
    same_cluster_distances <- dist_matrix_square[i, cluster_assignments == cluster & seq_along(cluster_assignments) != i]
    a <- if (length(same_cluster_distances) > 0) mean(same_cluster_distances) else 0
    
    # Calculate average distance to points in the nearest neighboring cluster (b)
    b <- min(sapply(unique(cluster_assignments), function(c) {
      if (c != cluster) mean(dist_matrix_square[i, cluster_assignments == c]) else Inf
    }))
    
    # Calculate silhouette value for the current data point
    silhouette_values[i] <- if (max(a, b) > 0) (b - a) / max(a, b) else 0
  }
  
  # Calculate mean silhouette score
  silhouette <- mean(silhouette_values, na.rm = TRUE)
  
  # Calculate within-cluster sum of squares (WCSS)
  wcss <- kmeans_result$tot.withinss
  
  # Return evaluation metrics with participant index
  return(list(participant_index = participant_index, silhouette = silhouette, wcss = wcss))
}

# Function to visualize cluster centers for comparison
visualize_cluster_centers <- function(results_list, optimal_results_list, participant_index) {
  # Set up the layout for multiple plots
  par(mfrow=c(1, 2))
  
  # Plot for optimal k
  plot(1:length(optimal_results_list[[participant_index]]$centers[,1]), optimal_results_list[[participant_index]]$centers[,1], type="b", main=paste("Cluster Centers (Optimal k) - Participant", participant_index),
       xlab="Cluster", ylab="Value")
}

# Function to analyze user distribution across clusters
analyze_user_distribution <- function(cluster_assignments_list, optimal_cluster_assignments_list, participant_index) {
  par(mfrow=c(1, 2))  # Set up the layout for multiple plots
  
  # Analyze user distribution for k clusters
  title_k <- paste("User Distribution (Participant", participant_index, ", k = 3)")
  user_distribution_k <- table(cluster_assignments_list[[participant_index]])
  barplot(user_distribution_k, main = title_k, xlab = "Cluster", ylab = "Number of Users")
  
  # Analyze user distribution for the optimal k
  title_optimal <- paste("User Distribution (Participant", participant_index, ", Optimal k)")
  user_distribution_optimal <- table(optimal_cluster_assignments_list[[participant_index]])
  barplot(user_distribution_optimal, main = title_optimal, xlab = "Cluster", ylab = "Number of Users")
}

# Function to analyze cluster results using aggregate function
analyze_cluster_results <- function(results_list, cluster_sizes) {
  for (i in 1:length(results_list)) {
    cat("Participant", i, "Cluster Sizes:\n")
    
    # Get the cluster sizes for the current participant
    sizes <- cluster_sizes[[i]]
    
    # Convert the cluster sizes to a data frame
    sizes_df <- data.frame(Cluster = names(sizes), Size = as.numeric(sizes))
    
    # Aggregate the sizes based on the cluster assignments
    aggregated_sizes <- aggregate(Size ~ Cluster, data = sizes_df, FUN = sum)
    print(aggregated_sizes)  # Print the aggregated sizes
  }
}

# Initialize lists to store cluster results
results_list <- list()
optimal_results_list <- list()

cluster_sizes <- list()  # Initialize cluster sizes list
optimal_cluster_sizes <- list()  # Initialize cluster sizes list for the optimal k

cluster_assignments_list <- list()  # Initialize list to store cluster assignments
optimal_cluster_assignments_list <- list()  # Initialize list to store cluster assignments for the optimal k

interests_list <- list()  # Initialize a list to store interests for each participant

# Loop through each participant to perform clustering
for (i in 1:22) {
  # Construct the filename
  filename <- paste0(i, ".csv")
  if (file.exists(filename)) {
    # Read and preprocess the data
    interests <- read_and_preprocess(filename)
    interests_list[[i]] <- interests  # Store interests for participant i
    
    # Find the optimal k value
    optimal_k <- find_optimal_k(interests)
    kmeans_result_optimal <- perform_kmeans(interests, optimal_k)
    optimal_results_list[[i]] <- kmeans_result_optimal
    optimal_cluster_sizes[[i]] <- table(kmeans_result_optimal$cluster)
    optimal_cluster_assignments <- user_identification(interests, kmeans_result_optimal)
    optimal_cluster_assignments_list[[i]] <- optimal_cluster_assignments
  } else {
    warning(paste("File for participant", i, "does not exist."))
  }
}

# Prompt user for participant index to evaluate
participant_index <- as.integer(readline(prompt = "Enter participant index: "))

# Evaluate clustering quality for the optimal k for a specific participant
evaluation_metrics_optimal <- evaluate_clustering(optimal_results_list[[participant_index]], interests_list[[participant_index]], participant_index)
print(paste("Participant", participant_index, "Evaluation Metrics for Optimal k:", toString(evaluation_metrics_optimal)))

# Analyze user distribution across clusters for the specified participant
analyze_user_distribution(cluster_assignments_list, optimal_cluster_assignments_list, participant_index)  # Analyze user distribution for the specified participant

# Analyze cluster results for optimal k across all participants
analyze_cluster_results(optimal_results_list, optimal_cluster_sizes)

