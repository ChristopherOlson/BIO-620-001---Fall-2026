dice <- c(4, 6, 8, 10, 12, 20)

benji <- c(8, 8, 5, 16, 6, 8)
marcus <- c(12, 8, 2, 1, 4, 7)
carrie <- c(5, 8, 7, 3, 2, 6)
lucy <- c(3, 5, 2, 3, 4, 3)

prior_benji <- c(0.25, 0.05, 0.10, 0.15, 0.20, 0.25)
prior_marcus <- c(0.04, 0.08, 0.16, 0.32, 0.25, 0.15)
prior_carrie <- c(0.20, 0.19, 0.20, 0.20, 0.01, 0.20)
prior_lucy <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.60)

dice_likelihood <- function(rolls, dice) {
  likelihood <- numeric(length(dice))
  
  for(i in 1:length(dice)) {
    if(max(rolls) <= dice[i]) {
      likelihood[i] <- (1 / dice[i])^length(rolls)
    } else {
      likelihood[i] <- 0
    }
  }
  
  return(likelihood)
}

like_benji <- dice_likelihood(benji, dice)
like_marcus <- dice_likelihood(marcus, dice)
like_carrie <- dice_likelihood(carrie, dice)
like_lucy <- dice_likelihood(lucy, dice)

post_benji <- like_benji * prior_benji
post_marcus <- like_marcus * prior_marcus
post_carrie <- like_carrie * prior_carrie
post_lucy <- like_lucy * prior_lucy

post_benji_norm <- post_benji / sum(post_benji)
post_marcus_norm <- post_marcus / sum(post_marcus)
post_carrie_norm <- post_carrie / sum(post_carrie)
post_lucy_norm <- post_lucy / sum(post_lucy)

results <- data.frame(
  Dice = dice,
  Benji_Likelihood = like_benji,
  Benji_Posterior = post_benji_norm,
  Marcus_Likelihood = like_marcus,
  Marcus_Posterior = post_marcus_norm,
  Carrie_Likelihood = like_carrie,
  Carrie_Posterior = post_carrie_norm,
  Lucy_Likelihood = like_lucy,
  Lucy_Posterior = post_lucy_norm
)

results

par(mfrow = c(2, 2))

plot(
  dice,
  log10(like_benji),
  type = "b",
  xlab = "Number of sides",
  ylab = "Log10 Likelihood",
  main = "Benji"
)

plot(
  dice,
  log10(like_marcus),
  type = "b",
  xlab = "Number of sides",
  ylab = "Log10 Likelihood",
  main = "Marcus"
)

plot(
  dice,
  log10(like_carrie),
  type = "b",
  xlab = "Number of sides",
  ylab = "Log10 Likelihood",
  main = "Carrie"
)

plot(
  dice,
  log10(like_lucy),
  type = "b",
  xlab = "Number of sides",
  ylab = "Log10 Likelihood",
  main = "Lucy"
)

par(mfrow = c(2, 2))

plot(
  dice,
  post_benji_norm,
  type = "b",
  xlab = "Number of sides",
  ylab = "Posterior Probability",
  main = "Benji"
)

plot(
  dice,
  post_marcus_norm,
  type = "b",
  xlab = "Number of sides",
  ylab = "Posterior Probability",
  main = "Marcus"
)

plot(
  dice,
  post_carrie_norm,
  type = "b",
  xlab = "Number of sides",
  ylab = "Posterior Probability",
  main = "Carrie"
)

plot(
  dice,
  post_lucy_norm,
  type = "b",
  xlab = "Number of sides",
  ylab = "Posterior Probability",
  main = "Lucy"
)

par(mfrow = c(1, 1))