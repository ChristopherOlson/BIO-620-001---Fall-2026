getwd()

list.files()

oat <- read.csv("oat.csv")
bacteria <- read.csv("bacteria.csv")

head(oat)
head(bacteria)

str(oat)
str(bacteria)

summary(oat)
summary(bacteria)

table(bacteria$species)
table(bacteria$diagnosis)
table(bacteria$species, bacteria$diagnosis)

stripchart(oat$grain,
           method = "stack",
           pch = 19,
           main = "Dot Chart of Grain",
           xlab = "Grain")

stripchart(oat$yield,
           method = "stack",
           pch = 19,
           main = "Dot Chart of Yield",
           xlab = "Yield")

boxplot(oat$grain,
        main = "Boxplot of Grain",
        ylab = "Grain")

boxplot(oat$yield,
        main = "Boxplot of Yield",
        ylab = "Yield")

hist(oat$grain,
     main = "Histogram of Grain",
     xlab = "Grain")

hist(oat$yield,
     main = "Histogram of Yield",
     xlab = "Yield")

plot(oat$grain,
     oat$yield,
     pch = 19,
     main = "Yield vs Grain",
     xlab = "Grain",
     ylab = "Yield")

stripchart(bacteria$toxin,
           method = "stack",
           pch = 19,
           main = "Dot Chart of Toxin",
           xlab = "Toxin")

stripchart(toxin ~ species,
           data = bacteria,
           method = "jitter",
           vertical = TRUE,
           pch = 19,
           main = "Toxin by Species",
           xlab = "Species",
           ylab = "Toxin")

stripchart(toxin ~ diagnosis,
           data = bacteria,
           method = "jitter",
           vertical = TRUE,
           pch = 19,
           main = "Toxin by Diagnosis",
           xlab = "Diagnosis",
           ylab = "Toxin",
           las = 2)

boxplot(toxin ~ species,
        data = bacteria,
        main = "Toxin by Species",
        xlab = "Species",
        ylab = "Toxin")

boxplot(toxin ~ diagnosis,
        data = bacteria,
        main = "Toxin by Diagnosis",
        xlab = "Diagnosis",
        ylab = "Toxin",
        las = 2)

hist(bacteria$toxin,
     main = "Histogram of Toxin",
     xlab = "Toxin")

aggregate(toxin ~ species,
          data = bacteria,
          FUN = mean)

aggregate(toxin ~ diagnosis,
          data = bacteria,
          FUN = mean)

aggregate(toxin ~ species + diagnosis,
          data = bacteria,
          FUN = mean)