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

  residual <- vector("list", length(x))

  for(i in 1:length(x)){
    residual[[i]] <- (as.numeric(x[[i]]$Fitted)-1) - x[[i]]$Prediction
  }
    #densityplot
    ggplot2::ggplot()+
         ggplot2::geom_density(aes(x = residual[[1]]))+
         ggplot2::geom_density(aes(x = residual[[2]]))+
         ggplot2::geom_density(aes(x = residual[[3]]))+
         ggplot2::geom_density(aes(x = residual[[4]]))
}
plot(model_cat(Potability~., water_potability))
