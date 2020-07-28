#' K-Nearest Neighbor that Combines Results from Two Distance Matrices
#'
#' This runs the KNN classification algorithm that accepts 2 distance matrices;
#' finding the nearest neighbors from both and returning the majority result
#' @param data The matrix containing the data to classify
#' @param first_distance The first distance matrix
#' @param second_distance The second distance matrix
#' @param training_labels The classification labels to compare to
#' @param K The value of K
#' @export

knn_combine <- function(data, first_distance, second_distance, training_labels, k) {
  # First, we'll check to see if all the data was entered
  if(!exists("data")) {
    stop("-- You have to include the data that we'll be working with -- \n" )
  }
  if(!exists("first_distance")) {
    stop("-- You have to include the first distance matrix  -- \n" )
  }
  if(!exists("second_distance")) {
    stop("-- You have to include the second distance matrix  -- \n" )
  }
  if(!exists("training_labels")) {
    stop("-- You have to include the true classification labels (so we can check whether the predicted labels match) -- \n")
  }
  if(k < 0) {
    stop("-- k cannot be 0 or less! -- \n")
  }
  #if(!isSymmetric(distance_matrix)) {
  #  stop("--The distance matrix is not symmetric -- \n")
  #}

  training_predictions <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    indices1 <- knn_for_one(i, first_distance, k)
    indices2 <- knn_for_one(i, second_distance, k)
    appended_indices <- rbind(indices1, indices2)
    label_table <- table(training_labels[appended_indices])
    training_predictions[i] <- sample(names(which(label_table == max(label_table))), size = 1)
  }
  return(training_predictions)
}

knn_for_one <- function(i, distance_matrix, k) {
  ordered_neighbors <- order(distance_matrix[i, ])
  return(ordered_neighbors[2:(k + 1)])
}
