#' rank
#'
#' Ranks several methods of classification based on their level of accuracy.
#' Returns a matrix of methods and ranked accuracies, with higher accuracies
#' listed first.
#'
#' @param formula The formula that the user wants to use for prediction.
#' @param train The training dataset used for prediction.
#' @param test The test dataset used for prediction.
#' @param response The response variable from the the test dataset. This is the
#' variable we are trying to predict.
#' @export

rank <- function(formula, train, test, response) {

  class_methods <- vector("list", 4)

  class_methods[[1]] <- MASS::lda(formula, data = train)
  class_methods[[2]] <- randomForest::randomForest(formula, data = train)
  class_methods[[3]] <- glm(formula, data = train)
  class_methods[[4]] <- EZtune::eztune(train[-train$response],
                                       train$response, method = "svm")

  predictions <- vector("list", length(class_methods))

  confusion <- vector("list", length(class_methods))

  accuracy <- matrix(NA, nrow = length(class_methods), 2)

  for (i in 1:length(class_methods)) {

    predictions[[i]] <- predict(class_methods[[i]], test[-test$response])
    confusion[[i]] <- table(test$response, predictions[[i]]$class)
    accuracy[i, 1] <- class(class_methods[[i]])
    accuracy[i, 2] <- (sum(diag(confusion[[i]])) / sum(confusion[[i]])) * 100

  }

  sort(accuracy)

}
