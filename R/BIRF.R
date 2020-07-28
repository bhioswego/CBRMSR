#' Balanced iterative random forest (Anaissi, Kennedy, Goyal and Catchpoole, 2013)
#'
#' This is a feature selection method that iteratively removes features with zero importance using Random Forest
#' It is mostly used in the selection module.
#' @param matrix Matrix with features in columns and samples in rows
#' @param class vector of classification labels for each sample
#' @export
#' @examples
#' \dontrun{
#' results <- BIRF(data, vector)
#' }
#' @import randomForest


BIRF <- function(matrix, class) {
  current.error <- 1
  previous.error <- 2
  loop_num <- 0
  previous_state <- matrix
  while(current.error < previous.error) {
    classifier <- randomForest(previous_state, class)
    impDF <- as.data.frame(importance(classifier))
    current_state <- previous_state[,impDF > 0]
    num_removed <- ncol(previous_state) - ncol(current_state)
    #cat("-- Number of features removed with BIRF: ",num_removed," -- \n")
    previous_state <- current_state
    errors <- classifier$err.rate
    previous.error <- current.error
    current.error <- mean(errors[,1])
  }
  output <- list(current_state, impDF)
  return(output)
}
