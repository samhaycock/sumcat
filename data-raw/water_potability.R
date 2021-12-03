## code to prepare `water_potability` dataset goes here
water <- read.csv("data-raw/water_potability.csv")

# Omitting any row that has NA values due to the fact that we have 1000+
#   observations.
water_clean <- na.omit(water)

#Set Potability as a Factor
water_clean$Potability <- factor(water_clean$Potability)

#Create a Test-dataset to check predictive

set.seed(1229)
n <- nrow(water_clean)
training.index <- sample(1:n, size = 0.8 * n)
water_potability <- water_clean[training.index, ]
water_test <- water_clean[-training.index, ]

usethis::use_data(water_potability, overwrite = TRUE)
usethis::use_data(water_test, overwrite = TRUE)
