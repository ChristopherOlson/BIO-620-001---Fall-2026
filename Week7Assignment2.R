species <- read.csv("species.csv")

species$pH <- as.factor(species$pH)

head(species)
str(species)
summary(species)

species.full <- glm(Species ~ pH + Biomass,
                    family = poisson(link = "log"),
                    data = species)

summary(species.full)

species.pH <- glm(Species ~ pH,
                  family = poisson(link = "log"),
                  data = species)

species.biomass <- glm(Species ~ Biomass,
                       family = poisson(link = "log"),
                       data = species)

species.null <- glm(Species ~ 1,
                    family = poisson(link = "log"),
                    data = species)

AIC(species.full,
    species.pH,
    species.biomass,
    species.null)

anova(species.full, test = "Chisq")

species.best <- step(species.full)

summary(species.best)

deviance(species.best) / df.residual(species.best)

plot(species$Biomass,
     species$Species,
     xlab = "Biomass",
     ylab = "Number of Species")

par(mfrow = c(2, 2))
plot(species.best)
par(mfrow = c(1, 1))