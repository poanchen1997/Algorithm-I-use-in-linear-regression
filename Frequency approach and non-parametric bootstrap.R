# Compare between "Frequency approach" and "Non-parametric bootstrap"

# Suppose our distribution fix as follows
n <- 100
x <- rnorm(n)
y <- x + rnorm(n)
# What will be the true beta_1? 
# It should be 1, because cov(y, x) = cov(x, x) = var(x)

# Frequence approach ------------------------------------------------------
# Pn, treating each observation (row) as having 1/n probability
# We can think of Pn as being a sample from P.
# As n grows large, Pn -> P 

# the true sampling distribution of beta.hat
B <- 10000
beta.hat <- rep(NA, B)
for (j in 1:B){
  n <- 100
  x <- rnorm(n)
  y <- x + rnorm(n)
  reg <-  lm(y ~ x)
  beta.hat[j] <- coef(reg)[2]
}

hist(beta.hat) # sample distribution of beta.hat
quantile(beta.hat, c(0.025, 0.975))

# non parametric bootstrap -------------------------------------------------
n <- 100
x <- rnorm(n)
y <- x + rnorm(n)
B <- 10000
beta.hat.boot <- rep(NA, B)
for (j in 1:B){
  idx <- sample(n, replace = TRUE)
  x.boot <- x[idx]
  y.boot <- y[idx]
  reg.boot <-  lm(y.boot ~ x.boot)
  beta.hat.boot[j] <- coef(reg.boot)[2]
}
hist(beta.hat.boot)
quantile(beta.hat.boot, c(0.025, 0.975))
# include true that beta is 1.

# conclusion:
# We can see that the frequency approach are more converge, but it takes more time 
# than the bootstrap, because it needs to generate 100 data for 10000 times, while 
# the other only need to generate the data once.