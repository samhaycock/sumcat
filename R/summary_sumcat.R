#' summary.sumcat
#'
#' Displays all the models one after the other, specifying the call that was
#'   made by displaying those results to the user in the console.
#'
#' @param sumcat This is a sumcat object which is a list of the different
#'   classification models embedded inside.
#' @examples
#'   x <- model_cat(Potability ~ ., water_potability, water_test)
#'   summary(x)
#' @export
summary.sumcat <- function(sumcat) {
  for (i in seq_len(length(sumcat))) {
    print()
    print(sumcat[[i]])
  }
}
