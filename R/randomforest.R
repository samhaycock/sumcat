library(sumcat)
library(randomForest)
library(party)

str(water_potability)

set.seed(1229)

model <- randomForest(
  formula = as.factor(Potability) ~ .,
  data = water_potability
)

print(model)
print(importance(model,type = 2))

#predict Potability from water_test
predict <- as.integer(predict(model, newdata=water_test[,-10]))-1

