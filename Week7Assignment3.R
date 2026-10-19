getwd()
list.files()

bolger <- read.csv("bolger.csv")

head(bolger)
str(bolger)
summary(bolger)

table(bolger$RODENTSP)

bolger.full <- glm(RODENTSP ~ PERSHRUB + DISTX + AGE,
                   family = binomial(link = "logit"),
                   data = bolger)

summary(bolger.full)

bolger.shrub <- glm(RODENTSP ~ PERSHRUB,
                    family = binomial(link = "logit"),
                    data = bolger)

bolger.distance <- glm(RODENTSP ~ DISTX,
                       family = binomial(link = "logit"),
                       data = bolger)

bolger.age <- glm(RODENTSP ~ AGE,
                  family = binomial(link = "logit"),
                  data = bolger)

bolger.null <- glm(RODENTSP ~ 1,
                   family = binomial(link = "logit"),
                   data = bolger)

AIC(bolger.full,
    bolger.shrub,
    bolger.distance,
    bolger.age,
    bolger.null)

anova(bolger.full, test = "Chisq")

bolger.best <- step(bolger.full)

summary(bolger.best)

exp(coef(bolger.best))

exp(confint(bolger.best))

bolger$predicted <- predict(bolger.best,
                            type = "response")

plot(bolger$PERSHRUB,
     bolger$RODENTSP,
     xlab = "Percent Shrub",
     ylab = "Rodent Presence (0/1)")

par(mfrow = c(2, 2))
plot(bolger.best)
par(mfrow = c(1, 1))