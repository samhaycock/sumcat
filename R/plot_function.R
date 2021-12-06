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
    residual[i] <- x[[i]]$Fitted - x[[i]]$Prediction

  #densityplot
  gglot2::ggplot(residual,aes(x=residual[[i]], col = class(x[[i]])))+
    ggplot2::geom_point()
    ggplot2::geom_density()
  }
}
