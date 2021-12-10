#'Plot
#'
#'Plotfunction which visualize residualplots from several methods for
#'classification. Returns a different colored plot
#'
#'@param x A sumcat list object which is the summary of classifications
#'  made by four different Regressionmethods. Can be reproduced with the
#'  model_cat() function
#'@return A density plot made with functions provided by the ggplot2 package
#'@description Takes the length of the argument to create a list object. Each
#'  item of the list object is the Substraction of the fitted values, obtained
#'  from x with the prediction values, short the residuals. The residuals are
#'  than plotted as a densitycurve over another, using the geom_density
#'  function from the ggplot2 package. Which classification model is
#'  represented by which colour is presented in a legend.
#'
#'@example
#' x <- model_cat(Potability~., water_potability)
#' plot(x)
#'
#'@export
<<<<<<< HEAD
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
=======
plot.sumcat <- function(x){

  residual <- vector("list", length(x))

  for(i in 1:length(x)){
    residual[[i]] <- (as.numeric(x[[i]]$Fitted)-1) - x[[i]]$Prediction
  }
    #densityplot
    ggplot2::ggplot()+
      ggplot2::theme_classic()+
         ggplot2::geom_density(ggplot2::aes(x = residual[[1]]), col = "purple")+
         ggplot2::geom_density(ggplot2::aes(x = residual[[2]]), col = "blue")+
         ggplot2::geom_density(ggplot2::aes(x = residual[[3]]), col = "green")+
         ggplot2::geom_density(ggplot2::aes(x = residual[[4]]), col= "yellow")
>>>>>>> f4c87d6294fbb247c22ec13187e6c7fbcef6c441
}

