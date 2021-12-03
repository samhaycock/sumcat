LDA <- MASS::lda(Potability ~ ., data = water_potability)
predicted_potability <- predict(LDA, water_test[, -10])

LDA_confusion <- table(water_test[, 10], predicted_potability$class)

accuracy_LDA <- (sum(diag(LDA_confusion)) / sum(LDA_confusion)) * 100

