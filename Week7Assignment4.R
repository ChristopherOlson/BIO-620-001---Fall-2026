getwd()
list.files()

molerat <- read.csv("molerat.csv")

head(molerat)
str(molerat)
summary(molerat)

molerat$caste <- as.factor(molerat$caste)

molerat.full <- glm(lnenergy ~ lnmass + caste,
                    family = Gamma(link = "log"),
                    data = molerat)

summary(molerat.full)

molerat.mass <- glm(lnenergy ~ lnmass,
                    family = Gamma(link = "log"),
                    data = molerat)

molerat.caste <- glm(lnenergy ~ caste,
                     family = Gamma(link = "log"),
                     data = molerat)

molerat.null <- glm(lnenergy ~ 1,
                    family = Gamma(link = "log"),
                    data = molerat)

AIC(molerat.full,
    molerat.mass,
    molerat.caste,
    molerat.null)

anova(molerat.full, test = "F")

molerat.best <- step(molerat.full)

summary(molerat.best)

plot(molerat$lnmass,
     molerat$lnenergy,
     xlab = "Log Mass",
     ylab = "Log Energy")

par(mfrow = c(2, 2))
plot(molerat.best)
par(mfrow = c(1, 1))