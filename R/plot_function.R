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

plot.sumcat <- function(x,train,test){
  class_methods <- vector("list", 3)

  class_methods[[1]] <- MASS::lda(x, data = train)
  class_methods[[2]] <- randomForest::randomForest(x, data = train)
  class_methods[[3]] <- glm(x, data = train)

  predictions <- vector("list", length(class_methods))
  residual <- vector("list", length(class_methods))

  for (i in 1:length(class_methods)) {

    predictions[[i]] <- predict(class_methods[[i]],
                                           newdata = test[,-10])
    residual[[i]] <- as.numeric(test[,10]) - as.numeric(predictions[[i]][[class]])
  }

  h <- do.call(cbind, residual)
  h.melt <- reshape2::melt(h)

  #densityplot
  gglot2::ggplot(h.melt,aes(x=class_methods[[1]],fill=residual, col = residual))+
    geom_density()+
    facet_grid(residual~.)+
    geom_rug()
}
