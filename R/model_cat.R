#' Modeling function for Ensemble of Categorical Analysis
#'
#' Creates a 'sumcat' class object that when summary is called the models
#'   for each type: Random Forest, Logistic Regression, LDA, SVM.
#' @param formula A formula statement
#' @param data A categorical data set specified by the user
#' @param test_data If user has already made a test data-set they may input it
#'   to allow the plot function to work, otherwise a test and training set will
#'   be created.
#'
#' @return A list object that contains the ensemble of models
#'
#' @examples
#' #Create a sumcat object
#' sumcat <- model_cat(Potability ~ ., water_potability, water_test)
#' sumcat
#'
#' @export
model_cat <- function(formula, data, test_data = NULL){
  if (is.null(test_data)){
    n <- nrow(water_clean)
    training.index <- sample(1:n, size = 0.8 * n)
    data <- data[training.index, ]
    test_data <- data[-training.index, ]
  }

  model_frame <- model.frame(formula, data = data)
  y <- model_frame[, 1]
  x <- model_frame[, 2:ncol(model_frame)]

  test_model_frame <- model.frame(formula, data = test_data)
  fitted.predict <- as.integer(test_model_frame[, 1]) - 1

  log_model <- glm(formula, data = data, family = "binomial")
  log_pred <- predict(log_model, newdata = test_data, type = "response")
  log_pred <- ifelse(log_pred < .5, 0, 1)
  log_model$Prediction <- log_pred
  log_model$Fitted <- fitted.predict

  rf_model <- randomForest::randomForest(formula, data = data)
  rf_pred <- as.integer(predict(rf_model, newdata = test_data)) - 1
  rf_model$Prediction <- rf_pred
  rf_model$Fitted <- fitted.predict

  svm_model <- EZtune::eztune(x, y, method = "svm")$model
  svm_pred <- as.integer(predict(svm_model, newdata = test_data)) - 1
  svm_model$Prediction <- svm_pred
  svm_model$Fitted <- fitted.predict

  lda_model <- MASS::lda(formula, data = data)
  lda_pred <- as.integer(predict(lda_model, newdata = test_data)$class) - 1
  lda_model$Prediction <- lda_pred
  lda_model$Fitted <- fitted.predict

  object <- list(log_model, rf_model, svm_model, lda_model)

  class(object) <- "sumcat"

  object
}
