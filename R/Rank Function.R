#' rank
#'
#' Ranks several methods of classification based on their level of accuracy.
#' Returns a matrix of methods and ranked accuracies, with higher accuracies
#' listed first.
#'
#' @param object
#' @export

rank.sumcat <- function(object) {

  num_methods <- 4

  confusion <- vector("list", num_methods)

  accuracy <- matrix(NA, nrow = num_methods, ncol = 2)

  for (i in 1:num_methods) {

    confusion[[i]] <- table(object[[i]]$Fitted, object[[i]]$Predictions)

    accuracy[i, 2] <- (sum(diag(confusion[[i]])) / sum(confusion[[i]])) * 100

  }

  rownames(accuracy) <- c("Logistic Regression", "Random Forest",
                          "Support Vector Machines",
                          "Linear Discriminant Analysis")
  colnames(accuracy) <- c("Classification Method", "Accuracy Percentage")
  sort(accuracy)

}

