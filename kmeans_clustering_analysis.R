# Clear the environment
rm(list = ls())

# Load required libraries
library(dplyr)
library(cluster)
library(ggplot2)
library(factoextra)

# Function to read and preprocess data
read_and_preprocess <- function(filename) {
  # Read the CSV file
  data <- read.csv(filename, header = FALSE)
  
  # Perform Z-score standardization directly on the interests data frame
  interests <- as.data.frame(lapply(data[-1], scale))  # Remove the first column (time step) before scaling
  
  return(interests)
}

# Function to perform k-means clustering
perform_kmeans <- function(interests, k) {
  # Perform k-means clustering
  set.seed(2345) # for reproducibility
  
  kmeans_result <- kmeans(interests, centers = k)
  
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
  # Calculate total within-cluster sum of squares for different values of k
  wss <- sapply(1:max_k, function(k) {
    kmeans_result <- kmeans(interests, centers = k)
    tot_withinss <- kmeans_result$tot.withinss
    return(tot_withinss)
  })
  
  # Find the index of the point with the maximum WCSS
  max_index <- which.max(wss)
  
  # Fit a line segment to the endpoints of the curve
  x1 <- 1
  y1 <- wss[1]
  x2 <- max_k
  y2 <- wss[max_k]
  
  # Calculate the perpendicular distance from each point on the curve to the line segment
  distances <- sapply(1:max_k, function(k) {
    distance <- perpendicular_distance(k, wss[k], x1, y1, x2, y2)
    return(distance)
  })
  
  # Find the index of the point with the greatest perpendicular distance
  elbow_point <- which.max(distances)
  
  return(elbow_point)
}

# Function for user identification (replace this with your actual user identification process)
user_identification <- function(interests, kmeans_result) {
  # Assign each user to the nearest cluster center
  cluster_centers <- kmeans_result$centers
  cluster_assignments <- apply(
    interests, 1, function(x) which.min(apply(cluster_centers, 1, function(y) sum((x - y)^2)))
    )
  
  return(cluster_assignments)
}

# Function to evaluate clustering quality
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
    
    # Calculate average distance to other points in the same cluster
    a <- mean(dist_matrix_square[i, cluster_assignments == cluster])
    
    # Calculate average distance to points in the nearest neighboring cluster
    b <- min(sapply(unique(cluster_assignments), function(c) mean(dist_matrix_square[i, cluster_assignments == c])))
    
    # Calculate silhouette value for the current data point
    silhouette_values[i] <- (b - a) / max(a, b)
  }
  
  # Calculate mean silhouette score
  silhouette <- mean(silhouette_values)
  
  # Calculate within-cluster sum of squares (WCSS)
  wcss <- kmeans_result$tot.withinss
  
  # Return evaluation metrics with participant index
  return(list(participant_index = participant_index, silhouette = silhouette, wcss = wcss))
}



# Function to visualize cluster centers
visualize_cluster_centers <- function(results_list, optimal_results_list, participant_index) {
  # Set up the layout for multiple plots
  par(mfrow=c(1, 2))  
  
  # Plot for k = 3
  plot(1:3, results_list[[participant_index]]$centers[,1], type="b", main=paste("Cluster Centers (k = 3) - Participant", participant_index),
       xlab="Cluster", ylab="Value")
  
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
    print(aggregated_sizes)
  }
}

# Initialize lists to store cluster results
results_list <- list()
optimal_results_list <- list()

cluster_sizes <- list()  # Initialize cluster sizes list
optimal_cluster_sizes <- list()  # Initialize cluster sizes list for the optimal k

cluster_assignments_list <- list()  # Initialize list to store cluster assignments
optimal_cluster_assignments_list <- list()  # Initialize list to store cluster assignments for the optimal k

# Iterate over the CSV files
for (i in 1:22) {
  # Construct the filename
  filename <- paste0(i, ".csv")
  # Read and preprocess the data
  interests <- read_and_preprocess(filename)
  
  # Perform k-means clustering with k = 3 initially
  kmeans_result <- perform_kmeans(interests, 3)
  # Store the cluster results for k = 3
  results_list[[i]] <- kmeans_result
  # Store cluster sizes for k = 3
  cluster_sizes[[i]] <- table(kmeans_result$cluster)
  # Perform user identification and cluster assignment for k = 3
  cluster_assignments <- user_identification(interests, kmeans_result)
  # Store cluster assignments for k = 3
  cluster_assignments_list[[i]] <- cluster_assignments
  
  # Find the optimal k value
  optimal_k <- find_optimal_k(interests)
  
  # Perform k-means clustering with optimal k-value
  kmeans_result_optimal <- perform_kmeans(interests, optimal_k)
  # Store the cluster results for the optimal k
  optimal_results_list[[i]] <- kmeans_result_optimal
  # Store cluster sizes for the optimal k
  optimal_cluster_sizes[[i]] <- table(kmeans_result_optimal$cluster)
  # Perform user identification and cluster assignment for the optimal k
  optimal_cluster_assignments <- user_identification(interests, kmeans_result_optimal)
  # Store cluster assignments for the optimal k
  optimal_cluster_assignments_list[[i]] <- optimal_cluster_assignments
  
}


participant_index <- as.integer(readline(prompt = "Enter participant index: "))


# Evaluate clustering quality for k = 3 for a specific participant
evaluation_metrics_k3 <- evaluate_clustering(results_list[[participant_index]], interests[[participant_index]], participant_index)
print(paste("Participant", participant_index, "Evaluation Metrics for k = 3:", toString(evaluation_metrics_k3)))

# Evaluate clustering quality for the optimal k for a specific participant
evaluation_metrics_optimal <- evaluate_clustering(optimal_results_list[[participant_index]], interests[[participant_index]], participant_index)
print(paste("Participant", participant_index, "Evaluation Metrics for Optimal k:", toString(evaluation_metrics_optimal)))

# Visualize cluster centers for both k = 3 and the 
optimal k of participate 1
visualize_cluster_centers(results_list, optimal_results_list, participant_index)

# Analyze user distribution across clusters of participate 1
analyze_user_distribution(cluster_assignments_list, optimal_cluster_assignments_list, participant_index)  # Analyze user distribution for the first participant


# Analyze cluster results for k=3
analyze_cluster_results(results_list, cluster_sizes)

# Analyze cluster results for optimal k
analyze_cluster_results(optimal_results_list, optimal_cluster_sizes)
