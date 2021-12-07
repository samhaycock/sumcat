LDA <- MASS::lda(Potability ~ ., data = water_potability)
predicted_potability <- predict(LDA, water_test[, -10])

# Accuracy (as a percentage)
LDA_confusion <- table(water_test[, 10], predicted_potability$class)

accuracy_LDA <- (sum(diag(LDA_confusion)) / sum(LDA_confusion)) * 100

# Residuals
actual <- water_test[, 10]

predicted <- as.integer(predicted_potability$class) - 1

LDA_residuals <- actual - predicted

