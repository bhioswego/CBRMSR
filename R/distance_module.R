#' Distance Matrices Module
#'
#' This module constructs the distance matrices for the confounding and predictor datasets.
#' Optionally, you can multiply the predictor distance matrix by the inverse
#' feature weights.
#'
#' @param CBRMSR A CBRMSR object
#' @param feature.weights Whether to multiply the predictor distance matrix by the feature weights computed in the selection module
#' @param categorical.similarity Either the Goodall3 or Lin1 algorithms from Boriah, Chandola, and Kumar (2008) can be used
#' @param confounding.type Presently, categorical is the only type that's supported but other data types are planned
#' @param feature.weights A boolean value of whether you want to use feature weights. The selection module must have been run prior to this module for this option to work.
#' @import stats sna cluster plyr nomclust tictoc
#' @importFrom analogue distance
#' @examples
#' \dontrun{
#' CBRMSR <- distance_module(CBRMSR, categorical.similarity = "Goodall", confounding.type = "categorical", feature.weights = TRUE)
#' }
#' @export

distance_module <- function(CBRMSR, categorical.similarity = c("Goodall", "Lin"), confounding.type = "categorical", feature.weights = TRUE) {


  tic("Distance Module Duration")
  cat("-- Computing distance matrices --\n")
  progress <- txtProgressBar(min = 0, max = CBRMSR$num, style = 3)


  for(i in 1:CBRMSR$num) {
    setTxtProgressBar(progress, i)
    training <- CBRMSR$training.sets[[i]]
    testing <- CBRMSR$testing.sets[[i]]

    testing <- testing[, match(colnames(training), colnames(testing))]

    CBRMSR$testing.sets[[i]] <- testing
    training_confounding <- CBRMSR$training.confounding.sets[[i]]
    testing_confounding <- CBRMSR$testing.confounding.sets[[i]]
    feature_weights <- CBRMSR$feature.weights[[i]]

    # confounding distance matrices
    # ----------------------------------------------


    combined_confounding <- rbind(testing_confounding, training_confounding)

    # Training
    if(confounding.type == "categorical") {
      if(categorical.similarity == "Goodall") {
        # Uses the Goodall3 algorithm in Boriah, Chandola, and Kumar (2008)
        goodall_similarity <- good3(combined_confounding)
        combined_confounding <- column_and_row_names(goodall_similarity, rownames(combined_confounding))



      }
      if(categorical.similarity == "Lin") {
        # Uses the Lin1 algorithm in Boriah, Chandola, and Kumar (2008)
        lin_similarity <- lin1(combined_confounding)
        combined_confounding <- column_and_row_names(lin_similarity, rownames(combined_confounding))


      }
    }
      training.clin.dist <- combined_confounding[match(rownames(training_confounding), rownames(combined_confounding)), ]
      training.clin.dist <- training.clin.dist[,match(rownames(training_confounding), colnames(training.clin.dist))]
      # we normalize simply to make the values more realistic and understandable using a function below
      #training.clin.dist <- normalize(training.clin.dist)
      #training.clin.dist <- diag.remove(training.clin.dist, 0.1)


      testing.clin.dist <- combined_confounding[match(rownames(testing_confounding), rownames(combined_confounding)), ]
      testing.clin.dist <- testing.clin.dist[,match(rownames(training_confounding), colnames(testing.clin.dist))]
      #testing.clin.dist <- normalize(testing.clin.dist)

    CBRMSR$training.confounding.distances[[i]] <- training.clin.dist
    CBRMSR$testing.confounding.distances[[i]] <- testing.clin.dist


    # predictor distance matrices
    # ----------------------------------------------

    # Testing predictor distances
    # We'll use two functions from other packages and include the feature weights if feature.weights is TRUE
    if(feature.weights == TRUE) {
      testing.array.dist <- distance(testing, training, method = "euclidean", weights = feature_weights)
      training.array.dist <- as.matrix(daisy(training, metric = "euclidean", weights = feature_weights$MeanDecreaseGini))
      #testing.array.dist <- normalize(testing.array.dist)
      #training.array.dist <- normalize(training.array.dist)
      #training.array.dist <- diag.remove(training.array.dist, 0.1)


      # If it's false, we'll simply calculate the distance matrices
    } else {
      training.array.dist <- as.matrix(dist(training, method = "euclidean"))
      testing.array.dist <- as.matrix(distance_between(testing, training))
      #testing.array.dist <- normalize(testing.array.dist)
      #training.array.dist <- normalize(training.array.dist)
      #training.array.dist <- diag.remove(training.array.dist, 0.1)

    }




    CBRMSR$training.predictor.distances[[i]] <- training.array.dist


    CBRMSR$testing.predictor.distances[[i]] <- testing.array.dist

    CBRMSR$testing.sets[[i]] <- testing
  }
  close(progress)

  toc()
  return(CBRMSR)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

column_and_row_names <- function(df, vector) {
  rownames(df) <- vector
  colnames(df) <- vector
  return(df)
}

