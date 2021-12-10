#'Plot
#'
#'Plotfunction which visualize residualplots from several methods for
#'classification. Returns a different colored plot
#'
#'@param x Variable with is dependent from the other variables:
#'  "Variable name ~."
#'@param train A dataset, which is used to train the models of several
#'classification methods
#'@param test A dataset, which is used to test the models in order to
#'create predictions
#'
#'@export
plot <- function(x){
  ggplot() +
    geom_density(aes(x = testing[[1]]$Fitted - testing[[1]]$Prediction),
                 col = "red") +
    geom_density(aes(x = testing[[2]]$Fitted - testing[[2]]$Prediction),
                 col = "blue") +
    geom_density(aes(x = testing[[3]]$Fitted - testing[[3]]$Prediction),
                 col = "green") +
    geom_density(aes(x = testing[[4]]$Fitted - testing[[4]]$Prediction),
                 linetype = "dashed")
}

