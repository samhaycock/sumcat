#' Plot
#'
#' Plot function which visualize residual plots from several methods for
#'   classification. Returns a plot with each residual density plot colored.
#'
#' @param x A sumcat list object which is the summary of classifications
#'   made by four different Classification methods. Can be reproduced with the
#'   model_cat() function.
#' @param y A null value in this case as all the information is stored inside
#'   the x object which is a sumcat object.
#' @param ... Parameters that can be passed on to later functions if needed.
#' @return A density plot made with functions provided by the ggplot2 package
#' @description Takes the fitted values(actual values) less the
#'   predicted values creating the residuals. These are then plotted as a
#'   density curve over one another, using the geom_density from ggplot2.
#'
#' @examples
#'  x <- model_cat(Potability~., water_potability, water_test)
#'  plot(x)
#' @import ggplot2
#' @export
plot.sumcat <- function(x, y, ...){
  ggplot() +
    geom_density(aes(x = x[[1]]$Fitted - x[[1]]$Prediction,
                     color = "Logistic Regression"), size = 1.05,
                 key_glyph = "path") +
    geom_density(aes(x = x[[2]]$Fitted - x[[2]]$Prediction,
                     color = "Random Forest"), size = 1.05,
                 key_glyph = "path") +
    geom_density(aes(x = x[[3]]$Fitted - x[[3]]$Prediction,
                     color = "SVM"), size = 1.05,
                 key_glyph = "path") +
    geom_density(aes(x = x[[4]]$Fitted - x[[4]]$Prediction,
                     color = "LDA"), size = 1.05,
                 linetype = "dashed", key_glyph = "path") +
    theme(legend.position = "right",
          plot.title = element_text(hjust = .5),
          text = element_text(size = 12)) +
    ylab("Density") +
    xlab("Residuals (Observed - Predicted)") +
    ggtitle("Density Curves of Residuals") +
    labs(colour = "Classification Method") +
    scale_linetype_manual(values = c("Logistic Regression" = "solid",
                                     "Random Forest" = "solid",
                                     "SVM" = "solid",
                                     "LDA" = "dashed"))
}

