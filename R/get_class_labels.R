
get_class_labels <- function(dataframe, classframe, labels, condition.symbol, control.symbol, percentile) {
  for(i in 1:ncol(dataframe)) {
    condition_indices <- which(labels == condition.symbol)
    control_indices <- which(labels == control.symbol)
    condition_names <- rownames(classframe)[condition_indices]
    control_names <- rownames(classframe)[control_indices]


    for(i in 1:ncol(dataframe)) {
      percentile <- as.numeric(quantile(dataframe[i,], percentile))
      indices <- which(dataframe[i,] < percentile)
      nearest <- rownames(dataframe)[indices]

    }
  }
}
