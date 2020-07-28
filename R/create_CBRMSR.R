#' Create a CBRMSR object
#'
#' CHANGE
#' @param predictor A dataset of predictor values
#' @param confounding An optional dataset of confounding variables (such as clinical data) to draw samples
#' from first.
#' @param classframe A two column dataframe of sample names and their classification label
#' @examples
#' \dontrun{
#' data(microarray)
#' data(clinical)
#' data(classframe)
#' CBRMSR <- create_CBRMSR(microarray, clinical, classframe)
#' }
#' @export


create_CBRMSR <- function(predictor, confounding, classframe) {
  object <- CBRMSR$new(predictor, confounding, classframe)
  return(object)
}
