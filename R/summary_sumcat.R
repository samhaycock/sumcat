#' summary.sumcat
#'
#' Displays all the models one after the other, specifying the call that was
#'   made by displaying those results to the user in the console.
#'
#' @param object This is a sumcat object which is a list of the different
#'   classification models embedded inside.
#' @param ... Parameters that can be passed on to future functions.
#' @examples
#'   x <- model_cat(Potability ~ ., water_potability, water_test)
#'   summary(x)
#' @export
summary.sumcat <- function(object, ...) {
  for (i in seq_len(length(object))) {
    print("\n")
    print(object[[i]])
  }
}
