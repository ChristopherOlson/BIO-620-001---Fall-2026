library(AICcmodavg)

cell <- read.csv("Cell_Inhibition_Schoenfeld.csv", check.names = FALSE)

cell$Inhibitor <- as.factor(cell$Inhibitor)
cell$`Cell Line` <- as.factor(cell$`Cell Line`)
cell$VHL <- as.factor(cell$VHL)

m0 <- lm(`PercentSurvival` ~ 1, data = cell)

m1 <- lm(`PercentSurvival` ~ Inhibitor, data = cell)

m2 <- lm(`PercentSurvival` ~ `Cell Line`, data = cell)

m3 <- lm(`PercentSurvival` ~ VHL, data = cell)

m4 <- lm(`PercentSurvival` ~ `Concentration uM`, data = cell)

m5 <- lm(
  `PercentSurvival` ~ Inhibitor + `Cell Line`,
  data = cell
)

m6 <- lm(
  `PercentSurvival` ~ Inhibitor + VHL,
  data = cell
)

m7 <- lm(
  `PercentSurvival` ~ Inhibitor + `Concentration uM`,
  data = cell
)

m8 <- lm(
  `PercentSurvival` ~ `Cell Line` + VHL,
  data = cell
)

m9 <- lm(
  `PercentSurvival` ~ `Cell Line` + `Concentration uM`,
  data = cell
)

m10 <- lm(
  `PercentSurvival` ~ VHL + `Concentration uM`,
  data = cell
)

m11 <- lm(
  `PercentSurvival` ~ Inhibitor + `Cell Line` + VHL,
  data = cell
)

m12 <- lm(
  `PercentSurvival` ~ Inhibitor + `Cell Line` + `Concentration uM`,
  data = cell
)

m13 <- lm(
  `PercentSurvival` ~ Inhibitor + VHL + `Concentration uM`,
  data = cell
)

m14 <- lm(
  `PercentSurvival` ~ `Cell Line` + VHL + `Concentration uM`,
  data = cell
)

m15 <- lm(
  `PercentSurvival` ~ Inhibitor + `Cell Line` + VHL + `Concentration uM`,
  data = cell
)

m16 <- lm(
  `PercentSurvival` ~ Inhibitor * `Concentration uM` + `Cell Line` + VHL,
  data = cell
)

models <- list(
  m0, m1, m2, m3, m4, m5, m6, m7, m8,
  m9, m10, m11, m12, m13, m14, m15, m16
)

model_names <- c(
  "Intercept",
  "Inhibitor",
  "Cell Line",
  "VHL",
  "Concentration",
  "Inhibitor + Cell Line",
  "Inhibitor + VHL",
  "Inhibitor + Concentration",
  "Cell Line + VHL",
  "Cell Line + Concentration",
  "VHL + Concentration",
  "Inhibitor + Cell Line + VHL",
  "Inhibitor + Cell Line + Concentration",
  "Inhibitor + VHL + Concentration",
  "Cell Line + VHL + Concentration",
  "Full Additive Model",
  "Inhibitor x Concentration"
)

aicc_values <- sapply(models, AICc)

results <- data.frame(
  Model = model_names,
  AICc = aicc_values
)

results$Delta_AICc <- results$AICc - min(results$AICc)

results <- results[order(results$AICc), ]

print(results)

best_model_number <- which.min(aicc_values)

best_model <- models[[best_model_number]]

model_names[best_model_number]

summary(best_model)

anova(best_model)