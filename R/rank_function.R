#' rank_methods
#'
#' Ranks several methods of classification based on their level of predictive
#' accuracy. Returns a matrix of methods and ranked accuracies from largest to
#' smallest. MUST run "model_cat" function prior to "rank_methods".
#'
#' @param object An object passed from the "model_cat" function.
#'
#' @examples
#' object <- model_cat(Potability ~ ., water_potability, water_test)
#' ranking(object)
#' @export

rank_methods <- function(object) {

  num_methods <- length(object)

  confusion <- vector("list", num_methods)

  accuracy <- matrix(NA, nrow = num_methods, ncol = 2)

  for (i in 1:num_methods) {

    confusion[[i]] <- table(object[[i]]$Fitted, object[[i]]$Prediction)


    accuracy[i, 2] <- round((sum(diag(confusion[[i]])) / sum(confusion[[i]])),
                            digits = 6) * 100

  }

  accuracy[1, 1] <- "Logistic Regression"
  accuracy[2, 1] <- "Random Forest"
  accuracy[3, 1] <- "Support Vector Machine"
  accuracy[4, 1] <- "Linear Discriminant Analysis"

  colnames(accuracy) <- c("Classification Method", "Accuracy (%)")
  accuracy <- as.data.frame(accuracy)
  accuracy[order(accuracy$`Accuracy (%)`, decreasing = TRUE), ]

}
