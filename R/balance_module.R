#' Class Balancing Module
#'
#' This module allows for the balancing of an unbalanced dataset through either SMOTE or ADASYN.
#' @param CBRMSR A CBRMSR object
#' @param method The balancing method. Currently smote, and ADASYN are supported
#' @param ratio Number between 0 and 1 that indicates the desired ratio between minority samples and majority samples
#' @import imbalance
#' @examples
#' \dontrun{
#' # Balancing with SMOTE
#' CBRMSR <- balance_module(CBRMSR, method = "smote", ratio = 0.8)
#' # Balancing with ADASYN
#' CBRMSR <- balance_module(CBRMSR, method = "ADASYN")
#' }
#' @export

balance_module <- function(CBRMSR, method = c("smote", "ADASYN"), ratio = 0.8) {
  if(ratio >= 1.0) {
    stop("--Ratio cannot be larger or equal to 1.0--\n")
  }

  tic("Balance Module Duration")
  cat("-- Running the class balancing module -- \n")
  cat("-- The Balance Module only operates on the training predictor data -- \n")
  cat("-- The Balance Module will overwrite the unbalanced training data with balanced training data. --  \n ")
  # Extracting the training data and their labels

  Sys.sleep(3)
  for(i in 1:CBRMSR$num) {
    training <- CBRMSR$training.sets[[i]]
    training.labels <- CBRMSR$training.labels[[i]]

    clinical <- CBRMSR$training.clinical.sets[[i]]
    #balance Oversample needs one dataframe, so we'll bind the training labels to the training data
    training <- cbind(training, training.labels)
    training <- as.data.frame(training)
    if(method == "smote") {
      training <- oversample(training, method = "SMOTE", classAttr = "training.labels", ratio = ratio)

    }
    if(method == "ADASYN") {
      training <- oversample(training, method = "ADASYN", classAttr = "training.labels")

    }
  # Undoing what we did to the training labels in order to run the oversampling
    training.labels <- as.factor(training$training.labels)
    balanced_training <- training[,-ncol(training)]
    CBRMSR$balanced.training.labels[[i]] <- training.labels
    CBRMSR$balanced.training.sets[[i]] <- balanced_training
    CBRMSR$balanced <- TRUE
  }
  # Determining how long it took to run
  toc()
  return(CBRMSR)
}

