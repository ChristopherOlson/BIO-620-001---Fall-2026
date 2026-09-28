# ============================================================
# Assignment 2: Argiope Fertilization
# Name: Christopher Olson
# ============================================================

argiope <- read.csv("Argiope_fertilization_small.csv")

head(argiope)
str(argiope)
summary(argiope)

colSums(is.na(argiope))

plot(argiope$InsertionDuration,
     argiope$NumFertilized,
     main = "Insertion Duration vs. Fertilized Eggs",
     xlab = "Insertion Duration",
     ylab = "Number of Fertilized Eggs",
     pch = 19)

simple_model <- lm(
  NumFertilized ~ InsertionDuration,
  data = argiope
)

summary(simple_model)

abline(simple_model, col = "red", lwd = 2)


full_model <- lm(
  NumFertilized ~ InsertionDuration +
    MaleSize +
    FemaleSize +
    NumEggs +
    NumSacs,
  data = argiope
)

summary(full_model)

anova(full_model)

reduced_model <- lm(
  NumFertilized ~ MaleSize +
    FemaleSize +
    NumEggs +
    NumSacs,
  data = argiope
)

summary(reduced_model)

anova(reduced_model, full_model)


par(mfrow = c(2, 2))

plot(full_model)

par(mfrow = c(1, 1))


library(car)

avPlot(full_model,
       variable = "InsertionDuration",
       main = "Adjusted Effect of Insertion Duration")



coef(summary(full_model))

summary(full_model)$r.squared
summary(full_model)$adj.r.squared

summary(full_model)$fstatistic

confint(full_model)

anova(reduced_model, full_model)