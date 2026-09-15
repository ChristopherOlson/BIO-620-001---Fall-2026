wheat <- read.csv("wheat.csv")

head(wheat)
str(wheat)
dim(wheat)
names(wheat)

colSums(is.na(wheat))


summary(wheat)

descriptive_stats <- data.frame(
  Variable = names(wheat),
  Mean = sapply(wheat, mean, na.rm = TRUE),
  SD = sapply(wheat, sd, na.rm = TRUE),
  Median = sapply(wheat, median, na.rm = TRUE),
  Min = sapply(wheat, min, na.rm = TRUE),
  Max = sapply(wheat, max, na.rm = TRUE)
)

descriptive_stats


par(mfrow = c(3, 3))

for (v in names(wheat)) {
  hist(
    wheat[[v]],
    main = paste("Histogram of", v),
    xlab = v,
    border = "black"
  )
}

par(mfrow = c(1, 1))


par(mfrow = c(3, 3))

for (v in names(wheat)) {
  boxplot(
    wheat[[v]],
    main = paste("Boxplot of", v),
    ylab = v
  )
}

par(mfrow = c(1, 1))


predictors <- setdiff(names(wheat), "yield")

par(mfrow = c(2, 3))

for (v in predictors) {
  plot(
    wheat[[v]],
    wheat$yield,
    xlab = v,
    ylab = "Yield",
    main = paste("Yield vs", v),
    pch = 19
  )
  
  abline(
    lm(wheat$yield ~ wheat[[v]]),
    lwd = 2
  )
}

par(mfrow = c(1, 1))


pairs(
  wheat,
  main = "Scatterplot Matrix for Wheat Dataset",
  pch = 19
)


cor_matrix <- cor(
  wheat,
  use = "complete.obs",
  method = "pearson"
)

round(cor_matrix, 3)

yield_correlations <- cor_matrix[, "yield"]

yield_correlations <- sort(
  yield_correlations,
  decreasing = TRUE
)

yield_correlations


cor.test(wheat$yield, wheat$winter)
cor.test(wheat$yield, wheat$ears)
cor.test(wheat$yield, wheat$pH)
cor.test(wheat$yield, wheat$P)
cor.test(wheat$yield, wheat$K)
cor.test(wheat$yield, wheat$Mg)


find_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  
  which(x < lower | x > upper)
}

outliers <- lapply(wheat, find_outliers)

outliers

for (v in names(wheat)) {
  
  rows <- find_outliers(wheat[[v]])
  
  if (length(rows) > 0) {
    
    cat("\nPossible outliers for", v, ":\n")
    
    print(
      data.frame(
        Row = rows,
        Value = wheat[[v]][rows]
      )
    )
  }
}


wheat_model <- lm(
  yield ~ winter + ears + pH + P + K + Mg,
  data = wheat
)

summary(wheat_model)


par(mfrow = c(2, 2))

plot(wheat_model)

par(mfrow = c(1, 1))


plot(
  fitted(wheat_model),
  wheat$yield,
  xlab = "Predicted Yield",
  ylab = "Observed Yield",
  main = "Observed vs Predicted Yield",
  pch = 19
)

abline(
  0,
  1,
  lwd = 2
)


confint(wheat_model)


anova(wheat_model)