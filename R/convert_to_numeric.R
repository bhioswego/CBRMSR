convert_to_numeric <- function(dataframe) {
  numeric <- sapply(dataframe, as.character)
  numeric <- data.matrix(dataframe)
  return(numeric)
}
