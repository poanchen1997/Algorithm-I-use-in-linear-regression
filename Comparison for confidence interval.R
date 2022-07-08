# This code is from our professor's lecture
# This script is used to compare the confidence from different approach

# sample size
n <- 100 

# true alpha
alpha <- 3

# true beta
beta <- 7

# true residual standard deviation 
s <- 2

# simulate data 
x <- rnorm(n)
y <- alpha + beta*x + rnorm(n, sd = s)

# fit model
ols.model <- lm(y ~ x)

# estimated coefficients
coef(ols.model) 

# the nonparametric bootstrap
## number of bootstrap replications
B <- 10000

## vectors to store results
alpha.boot <- rep(NA, B)
beta.boot  <- rep(NA, B)

# bootstrap samples
for(j in 1:B){
  # sample cases (rows)
  idx      <- sample(n, replace = T) 
  
  # fit OLS to bootstrapped cases
  x.boot   <- x[idx]
  y.boot   <- y[idx]
  ols.boot <- lm(y.boot ~ x.boot)
  
  # save results
  alpha.boot[j] <- coef(ols.boot)[1]
  beta.boot[j]  <- coef(ols.boot)[2]
}

## confidence interval from percentile
# set significance level
alpha <- 0.05

# Percentile CI -------------------------------------------

## CI for alpha
cat("Percentile CI for alpha:")
quantile(alpha.boot, c(alpha/2, 1-alpha/2))

## CI for beta
cat("Percentile CI for beta:")
quantile(beta.boot, c(alpha/2, 1-alpha/2))

# Confidence interval from Normal approximation ---------------
## critical threshold
z <- qnorm(1-alpha/2)

## compute bootstrap standard errors
sd.boot.a <- sd(alpha.boot)
sd.boot.b <- sd(beta.boot)

## CI for alpha
cat("Normal approximation CI for alpha:")
c(coef(ols.model)[1] - z*sd.boot.a, coef(ols.model)[1] + z*sd.boot.a)

## CI for beta
cat("Normal approximation CI for beta:")
c(coef(ols.model)[2] - z*sd.boot.b, coef(ols.model)[2] + z*sd.boot.b)

# confidence interval from parametric approach ------------------
confint(ols.model, level = 1-alpha)

# Conclusion: --------------------------------------------------

## We can see that for this example, the CI is biggest when using 
## parametric approach, and then goes normal approximation, 
## non-parametric goes the last. The two approach from non-parametric
## are almost the same, while parametric has a more difference.