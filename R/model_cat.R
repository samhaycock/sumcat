#' Modeling function for Ensemble of Categorical Analysis
#'
#' Creates a 'sumcat' class object that when summary is called the models
#'   for each type: Random Forest, Logistic Regression, LDA, SVM.
#' @param formula A formula statement
#' @param data A categorical data set specified by the user
model_cat <- function(formula, data){
  log_model <- glm(formula = formula, data = data, family = "binomial")

  rf_model <- randomForest::randomForest(factor(Potability) ~.,
                                         data = water_potability)
}
