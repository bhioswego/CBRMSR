#' CBRMSR Class
#'
#' This is a class object to store all of the results performed with the modules of this package
#' @import R6

CBRMSR <- R6Class(classname = "CBRMSR",
    public = list(
        predictor = NULL,
        #' @field predictor the dataset of predictor variables
        confounding = NULL,
        #' @field confounding the dataset of confounding variables
        classframe = NULL,
        #' @field classframe a column of sample names and a column of their class labels
        num = NULL,
        #' @field num num is the number of folds when folding
        training.sets = NULL,
        #' @field training.sets all of the training sets after splitting or folding
        testing.sets = NULL,
        #' @field testing.sets all of the testing sets after splitting or folding
        training.labels = NULL,
        #' @field training.labels the classification labels for the training sets
        testing.labels = NULL,
        #' @field testing.labels the classification labels for the testing sets
        training.confounding.sets = NULL,
        #' @field training.confounding.sets subsetted confounding data for the training sets
        testing.confounding.sets = NULL,
        #' @field testing.confounding.sets subsetted confounding data for the testing sets
        selected.features = NULL,
        #' @field selected.features which features were selected after feature selected
        feature.weights = NULL,
        #' @field feature.weights feature weights after using BIRF
        balanced = FALSE,
        #' @field balanced a boolean value of whether or not class balancing was applied
        balanced.training.labels = NULL,
        #' @field balanced.training.labels the classification labels of the training set after balancing
        balanced.training.sets = NULL,
        #' @field balanced.training.sets the training datsets after balancing
        training.confounding.distances = NULL,
        #' @field training.confounding.distances the distance matrices of the training confounding data
        testing.confounding.distances = NULL,
        #' @field testing.confounding.distances the distance matrices of the testing confounding data
        training.predictor.distances = NULL,
        #' @field training.predictor.distances the distance matrices of the training predictor data
        testing.predictor.distances = NULL,
        #' @field testing.predictor.distances the distance matrices of the testing predictor data
        training.confusion.matrices = NULL,
        #' @field training.confusion.matrices confusion matrices for the training sets
        training.predicted.labels = NULL,
        #' @field training.predicted.labels predicted classification labels for the training data
        testing.confusion.matrices = NULL,
        #' @field testing.confusion.matrices confusion matrices for the testing sets
        testing.predicted.labels = NULL,
        #' @field testing.predicted.labels predicted classification labels for the testing data
        #' @description
        #' Create a new CBRMSR object.
        #' @param predictor A dataframe of predictor variables
        #' @param confounding A dataframe of confounding variables
        #' @param classframe A 2 column dataframe of sample names and classification labels
        #' @return A new 'CBRMSR' object.
        initialize = function(predictor, confounding, classframe) {
            self$predictor <- predictor
            self$confounding <- confounding
            self$classframe <- classframe
        }
    )
)
