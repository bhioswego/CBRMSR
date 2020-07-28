#' Data Splitting Module
#'
#' This function splits the data into a training set and a testing set after shuffling the rows
#' @param CBRMSR A CBRMSR object
#' @param SplitPercent Percentage of the training data (requires a decimal less than 1.0)
#' @examples
#' \dontrun{
#' CBRMSR <- splitting_module(CBRMSR, SplitPercent = 0.75)
#' }
#' @export

splitting_module <- function(CBRMSR, SplitPercent = 0.75) {
  if(SplitPercent > 1.0) {
    stop("-- The percent of the training data cannot be larger than 1.0 -- \n")
  }

  set.seed(10)

  # Retrieving the data we'll need
  predictor <- CBRMSR$predictor
  classframe <- CBRMSR$classframe
  confounding <- CBRMSR$confounding
  n = nrow(predictor)

  cat("-- Running the data splitting module-- \n")
  tic("Splitting Module Duration")

  # Creating empty lists to store the datasets and their labels in
  training_sets <- list()
  testing_sets <- list()

  training_labels <- list()
  testing_labels <- list()

  training_confounding_sets <- list()
  testing_confounding_sets <- list()

  # Splitting
  # ----------------------------------------------------
  # Extracting the data from the CBRMSR object


    # Splitting through boolean logic
    split = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(SplitPercent, (1-SplitPercent)))
    training <- predictor[split, ]
    testing <- predictor[!split, ]

    # We'll need the classification labels of these sets to determine accuracy later
    training_labels[[1]] <- factor(classframe[match(rownames(training), rownames(classframe)), ])
    testing_labels[[1]] <- factor(classframe[match(rownames(testing), rownames(classframe)), ])

    # Subset the confounding data
    training_confounding <- confounding[match(rownames(training), rownames(confounding)), ]
    testing_confounding <- confounding[match(rownames(testing), rownames(confounding)), ]


    # Add them to the lists
    training_sets[[1]] <- training
    testing_sets[[1]] <- testing

    training_confounding_sets[[1]] <- training_confounding
    testing_confounding_sets[[1]] <- testing_confounding

    # Now we'll add them to the CBRMSR class object
    CBRMSR$num <- 1
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
