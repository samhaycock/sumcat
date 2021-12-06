#'Plot
#'
#'Plotfunction which visualize residualplots from several methods for
#'classification. Returns a different colored plot
#'
#'@param x a List of Lists, which are summaries of each method for
#'   classification
#' @param y A null argument as the information will be pulled from the x
#'   component
#'

plot.sumcat <- function(x, y, ...){
  class_methods <- vector("list", 3)

  class_methods[[1]] <- MASS::lda(formula, data = train)
  class_methods[[2]] <- randomForest::randomForest(formula, data = train)
  class_methods[[3]] <- glm(formula, data = train)

  predictions <- vector("list", length(class_methods))
  residual <- vector("list", length(class_methods))

  for (i in 1:length(class_methods)) {

    predictions[[i]] <- predict(class_methods[[i]], test[-test$response])
    residual[[i]] <- class_methods[[i]] - predictions[[i]]
  }

  h <- do.call(cbind, residual)
  h.melt <- reshape2::melt(h)

  #densityplot
  gglot2::ggplot(h.melt,aes(x=class_methods[[1]],fill=residual, col = residual))+
    geom_density()+
    facet_grid(residual~.)+
    geom_rug()
}
