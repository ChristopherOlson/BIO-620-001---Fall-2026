bolger <- read.csv("Datasets/bolger.csv")

model <- glm(
  RODENTSP ~ PERSHRUB + DISTX + AGE,
  data = bolger,
  family = binomial
)

summary(model)

dev.new()

plot(
  bolger$PERSHRUB,
  jitter(bolger$RODENTSP, amount = 0.04),
  pch = 19,
  col = "steelblue",
  xlab = "Percent Shrub Cover",
  ylab = "Native Rodent Occurrence",
  main = "Shrub Cover and Native Rodent Occurrence",
  ylim = c(-0.1, 1.1),
  yaxt = "n"
)

axis(2, at = c(0, 1),
     labels = c("Absent", "Present"))

newdata <- data.frame(
  PERSHRUB = seq(
    min(bolger$PERSHRUB),
    max(bolger$PERSHRUB),
    length.out = 200
  ),
  DISTX = mean(bolger$DISTX),
  AGE = mean(bolger$AGE)
)

predictions <- predict(
  model,
  newdata = newdata,
  type = "response"
)

lines(
  newdata$PERSHRUB,
  predictions,
  col = "red",
  lwd = 3
)

dev.new()

boxplot(
  PERSHRUB ~ RODENTSP,
  data = bolger,
  names = c("Absent", "Present"),
  col = c("lightblue", "lightgreen"),
  main = "Shrub Cover by Rodent Occurrence",
  xlab = "Native Rodent Occurrence",
  ylab = "Percent Shrub Cover"
)