#' Modeling function for Ensemble of Categorical Analysis
#'
#' Creates a 'sumcat' class object that when summary is called the models
#'   for each type: Random Forest, Logistic Regression, LDA, SVM.
#' @param formula A formula statement
#' @param data A categorical data set specified by the user
#'
#' @return A list object that contains the ensemble of models
#' @export
model_cat <- function(formula, data, test_data = water_test){
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
