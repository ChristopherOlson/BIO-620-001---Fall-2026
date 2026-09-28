# ============================================================
# Assignment 3: Quine Dataset
# Name: Christopher Olson
# ============================================================

quine <- read.csv("quine.csv")

head(quine)
str(quine)
summary(quine)
colSums(is.na(quine))

quine$Eth <- as.factor(quine$Eth)
quine$Sex <- as.factor(quine$Sex)
quine$Age <- as.factor(quine$Age)
quine$Lrn <- as.factor(quine$Lrn)

table(quine$Eth)
table(quine$Sex)
table(quine$Age)
table(quine$Lrn)

aggregate(Days ~ Eth, data = quine, FUN = mean)
aggregate(Days ~ Sex, data = quine, FUN = mean)
aggregate(Days ~ Age, data = quine, FUN = mean)
aggregate(Days ~ Lrn, data = quine, FUN = mean)

par(mfrow = c(2, 2))

boxplot(Days ~ Eth, data = quine,
        main = "Absences by Ethnicity",
        xlab = "Ethnicity", ylab = "Days Absent")

boxplot(Days ~ Sex, data = quine,
        main = "Absences by Sex",
        xlab = "Sex", ylab = "Days Absent")

boxplot(Days ~ Age, data = quine,
        main = "Absences by Age",
        xlab = "Age Group", ylab = "Days Absent")

boxplot(Days ~ Lrn, data = quine,
        main = "Absences by Learner Status",
        xlab = "Learner Status", ylab = "Days Absent")

par(mfrow = c(1, 1))

quine_model <- aov(
  Days ~ Eth + Sex + Age + Lrn,
  data = quine
)

summary(quine_model)
summary.lm(quine_model)

tukey_results <- TukeyHSD(quine_model)

tukey_results
tukey_results$Eth
tukey_results$Sex
tukey_results$Age
tukey_results$Lrn

plot(tukey_results)

par(mfrow = c(2, 2))
plot(quine_model)
par(mfrow = c(1, 1))

shapiro.test(residuals(quine_model))

library(car)

leveneTest(
  Days ~ interaction(Eth, Sex, Age, Lrn),
  data = quine
)