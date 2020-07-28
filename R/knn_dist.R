#' K-Nearest Neighbor with a Distance Matrix
#'
#' This runs the KNN classification algorithm and accepts a distance matrix as an argument
#' @param data The matrix containing the data to classify
#' @param distance_matrix The distance matrix
#' @param training.labels The classification labels to compare to
#' @param K The value of K
#' @export

knn_dist <- function(data, distance_matrix, training.labels, k) {
  # First, we'll check to see if all the data was entered
  if(!exists("data")) {
    stop("-- You have to include the data that we'll be working with -- \n" )
  }
  if(!exists("distance_matrix")) {
    stop("-- You have to include the distance matrix  -- \n" )
  }
  if(!exists("training.labels")) {
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
    indices <- knn_for_one(i, distance_matrix, k)
    label_table <- table(training.labels[indices])
    training_predictions[i] <- sample(names(which(label_table == max(label_table))), size = 1)
  }
  return(training_predictions)
}

knn_for_one <- function(i, distance_matrix, k) {
  ordered_neighbors <- order(distance_matrix[i, ])
  return(ordered_neighbors[2:(k + 1)])
}
