# This script is from our professor's lecture

library(sandwich)
library(car)

# data generation
set.seed(6)
n <- 1e2
x <- rnorm(n)
y <- rnorm(n, mean = 0.5 + x, sd = sqrt(1 + 5*x^2))
m <- lm(y ~ x)
confint(m)  # Note that in this example, CI is not cover the true value(1)

# Robust regression
S(m, vcov = vcovHC(m, type = "HC0"))
Confint(m, vcov. = vcovHC(m, type = "HC0"))
 # Now the CI cover the true value
