a <- 52.22
b <- 9.52

curve(dbeta(x, a, b),
      from = 0, to = 1,
      xlab = "Proportion",
      ylab = "Density",
      main = "Prior Distribution")

successes <- 45
total <- 50

curve(dbinom(successes, total, x),
      from = 0, to = 1,
      xlab = "Proportion",
      ylab = "Likelihood",
      main = "Likelihood")

posterior_a <- a + successes
posterior_b <- b + (total - successes)

posterior_a
posterior_b

curve(dbeta(x, posterior_a, posterior_b),
      from = 0, to = 1,
      xlab = "Proportion",
      ylab = "Density",
      main = "Posterior Distribution")

posterior_mean <- posterior_a / (posterior_a + posterior_b)
posterior_mean