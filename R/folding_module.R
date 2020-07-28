#' K Folding Module
#'
#' This function creates relatively equal folds
#' @param CBRMSR A CBRMSR object
#' @param Folds Number of folds (for cross-validation)
#' @param LOOC A boolean value of whether to use leave one out cross validation. This feature is not supported yet.
#' @examples
#' \dontrun{
#' CBRMSR <- folding_module(CBRMSR, folds = 10, LOOC = FALSE)
#' }
#' @import stats
#' @export

folding_module <- function(CBRMSR, Folds = 10, LOOC = FALSE) {
  # First we'll retrieve the data that we need
  predictor <- CBRMSR$predictor
  classframe <- CBRMSR$classframe
  confounding <- CBRMSR$confounding
  CBRMSR$num <- Folds
  n = nrow(predictor)

  tic("Folding Module Duration")
  if(Folds > n) {
    stop("-- Number of folds cannot be greater than the number of samples -- \n")
  }
  cat("-- Running the data folding module-- \n")

  # Create some empty lists to store the data and their labels
  training_sets <- list()
  testing_sets <- list()

  training_labels <- list()
  testing_labels <- list()

  training_confounding_sets <- list()
  testing_confounding_sets <- list()

  if(LOOC == FALSE) {
  # K Fold
  # ----------------------------------------------------
    # Shuffle the data and split it into relatively equal folds
    shuffled_predictor <- predictor[sample(nrow(predictor)),]
    folds <- cut(seq(1,nrow(shuffled_predictor)),Folds,labels=FALSE)

  cat("-- Splitting your data into ",Folds," folds -- \n")
    for(i in 1:Folds){
      # Using the fold indices above, subset the predictor data
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- predictor[testIndexes, ]
      trainData <- predictor[-testIndexes, ]

      # Add them to their respective lists
      training_sets[[i]] <- trainData
      testing_sets[[i]] <- testData

      # Subset the classframe to only the labels needed for a particular training and testing set
      training_labels[[i]] <- factor(classframe[match(rownames(trainData), rownames(classframe)), ])
      testing_labels[[i]] <- factor(classframe[match(rownames(testData), rownames(classframe)), ])

      # Subset the confounding data to match a particular training and testing set
      training_confounding <- confounding[match(rownames(trainData), rownames(confounding)), ]
      testing_confounding <- confounding[match(rownames(testData), rownames(confounding)), ]

      # Add the confounding data to their lists
      training_confounding_sets[[i]] <- training_confounding
      testing_confounding_sets[[i]] <- testing_confounding
    }
  }

  # Add everything to the CBRMSR class object
  CBRMSR$training.sets <- training_sets
  CBRMSR$testing.sets <- testing_sets
  CBRMSR$training.labels <- training_labels
  CBRMSR$testing.labels <- testing_labels
  CBRMSR$training.confounding.sets <- training_confounding_sets
  CBRMSR$testing.confounding.sets <- testing_confounding_sets

  # Last, we'll determine duration
  toc()
  return(CBRMSR)
}
