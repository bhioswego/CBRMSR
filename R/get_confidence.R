

get_confidence <- function(dataframe, classframe) {
  confidence_frame <- matrix(0, ncol = 1, nrow = nrow(dataframe))
  rownames(confidence_frame) <- rownames(dataframe)
  colnames(confidence_frame) <- "confidence"
  for(i in 1:ncol(dataframe)) {
    sample <- colnames(dataframe)[i]
    index <- which(rownames(classframe) == sample)
    classification <- classframe[index,]
    classframe_subset <- classframe[-index,]
    same_class <- which(classframe == classification)
    different_class <- which(!classframe == classification)
    same_class_samples <- rownames(classframe)[same_class]
    different_class_samples <- rownames(classframe)[different_class]

    column <- as.data.frame(dataframe[,i])
    rownames(column) <- rownames(dataframe)
    names(column) <- rownames(column)[1]
    column <- as.data.frame(column[-1,])

    same_class_column <- column[same_class,]
    different_class_column <- column[different_class,]
    same_class_column <- na.omit(same_class_column)
    different_class_column <- na.omit(different_class_column)

    same_class_average <- mean(same_class_column)
    different_class_average <- mean(different_class_column)

    #confidence <- different_class_average - same_class_average
    confidence <- same_class_average - different_class_average
    confidence_frame[i,] <- confidence
    confidence_frame <- normalize(confidence_frame)
  }
  return(confidence_frame)
}
