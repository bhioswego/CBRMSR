#' Feature Selection Module
#'
#' @param CBRMSR A CBRMSR object
#' @param method The method for feature selection. Options are BIRF (Balanced Iterative Random Forest) and rknn (random KNN)
#' @importFrom rknn rknn
#' @importFrom randomForest randomForest
#' @examples
#' \dontrun{
#' # Feature selection with Balanced Iterative Random Forest
#' CBRMSR <- selection_module(CBRMSR, method = "BIRF")
#' # Feature selection with random KNN
#' CBRMSR <- selection_module(CBRMSR, method = "rknn")
#' }
#' @export

selection_module <- function(CBRMSR, method = c("BIRF", "rknn")) {
  tic("Selection Module Duration")
  cat("-- Running the selection module -- \n")

  progress <- txtProgressBar(min = 0, max = CBRMSR$num, style = 3)

  for(i in 1:CBRMSR$num) {
    # Iterate through each fold (1 if splitting was used)
    setTxtProgressBar(progress, i)
    # BIRF is a Balanced Iterative Random Forest that removes non-important features until the model is overfit
    if(method == "BIRF") {
      # The BIRF function requires a matrix and a dataframe so we'll convert before sending into that function
      if(CBRMSR$balanced == TRUE) {
        # If the classes have been balanced, we'll have different training sets and labels
        mat <- as.matrix(CBRMSR$balanced.training.sets[[i]])
        class <- CBRMSR$balanced.training.labels[[i]]
        class <- as.data.frame(class)
        # Sending a matrix and a vector of classification labels to the BIRF function
        results <- BIRF(mat, class$class)
        df <- results[[1]]
        # we save the importance values to use for weights
        importance <- results[[2]]
        training_set <- CBRMSR$training.sets[[i]]
        training_set <- training_set[,match(colnames(df), colnames(training_set))]
        CBRMSR$training.sets[[i]] <- training_set
        CBRMSR$selected.features[[i]] <- colnames(mat)
        CBRMSR$feature.weights[[i]] <- importance
      }
      if(CBRMSR$balanced == FALSE) {
        mat <- as.matrix(CBRMSR$training.sets[[i]])
        class <- CBRMSR$training.labels[[i]]
        class <- as.data.frame(class)
        results <- BIRF(mat, class$class)
        df <- results[[1]]
        importance <- results[[2]]
        CBRMSR$training.sets[[i]] <- df
        CBRMSR$selected.features[[i]] <- colnames(df)
        CBRMSR$feature.weights[[i]] <- importance
      }
    }

    if(method == "rknn") {
      # rknn is random KNN. Li, Harner, and Adjeroh (2011)
      if(CBRMSR$balanced == TRUE) {
        training <- CBRMSR$balanced.training.sets[[i]]
        class <- CBRMSR$balanced.training.labels[[i]]
        r <- r(p = ncol(training), m = floor(sqrt(ncol(training))))
        rknnresults <- rknnBeg(training, y = class, k = 3, r = r)
        best <- as.data.frame(bestset(rknnresults, criterion = "mean_support"))
        # rknn uses a support criterion as a measure of importance
        support <-rknnSupport(training, y = class, k = 3, r = r)
        support <- support$support
        training <- training[,match(best$best, colnames(training))]
        CBRMSR$training.sets[[i]] <- training
        CBRMSR$selected.features[[i]] <- colnames(training)
        CBRMSR$feature.weights[[i]] <- support
      }

      if(CBRMSR$balanced == FALSE) {
          training <- CBRMSR$training.sets[[i]]
          class <- CBRMSR$training.labels[[i]]
          r <- r(p = ncol(training), m = floor(sqrt(ncol(training))))
          rknnresults <- rknnBeg(training, y = class, k = 3, r = r)
          best <- as.data.frame(bestset(rknnresults, criterion = "mean_support"))
          support <-rknnSupport(training, y = class, k = 3, r = r)
          support <- support$support
          training <- training[,match(best$best, colnames(training))]
          CBRMSR$training.sets[[i]] <- training
          CBRMSR$selected.features[[i]] <- colnames(training)
          CBRMSR$feature.weights[[i]] <- support
      }
    }

  }

  cat("-- \n The Feature Selection Module has finished -- \n")
  close(progress)
  toc()
  return(CBRMSR)
}
