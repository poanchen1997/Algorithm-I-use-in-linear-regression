# Omitted variable bias
# this theorem is used to find out the difference when you
# omit one variable.

# Still use the same distribution we use previously
x <- rnorm(10000, 1, 1)  # the third parameter is SD, not Var.
z <- rnorm(10000, 3, 2)
e <- rnorm(10000, 0, 1)
beta_0 <- 2
beta_1 <- 2
beta_2 <- 4
y <- beta_0 + beta_1*x + beta_2*z + e
ols <- lm(y ~ x+z)
coef(ols)
# what if you just model y on x
ols3 <- lm(y ~ x)
ols3  # it still be 2, why?
# remember the omitting variable bisa throrem
# for this case
# \beta_{1r} = \beta_1 + beta_2*\frac{Cov(X, Z)}{Var(X)}
# in out case Cov(X, Z) = 0, so \beta_{1r} = \beta{1}

# what if z and x have relationships --------------------------------------
x <- rnorm(10000, 1, 1)  # the third parameter is SD, not Var.
z <- x + rnorm(10000, 3, 2)
e <- rnorm(10000, 0, 1)
beta_0 <- 2
beta_1 <- 2
beta_2 <- 4
y <- beta_0 + beta_1*x + beta_2*z + e
ols <- lm(y ~ x+z)
coef(ols)

ols3 <- lm(y ~ x)
ols3 # it become almost 6. Why?
# \beta_{1r} = \beta_{1} + beta_2*\frac{Cov(X, Z)}{Var(X)}
#            =     2     +   4   *\frac{1}{1}
#            =     6

# how about you change the relationship between z and x -------------------
x <- rnorm(10000, 1, 1)  # the third parameter is SD, not Var.
z <- 2*x + rnorm(10000, 3, 2)
e <- rnorm(10000, 0, 1)
beta_0 <- 2
beta_1 <- 2
beta_2 <- 4
y <- beta_0 + beta_1*x + beta_2*z + e
ols <- lm(y ~ x+z)
coef(ols)

ols3 <- lm(y ~ x)
ols3 # it also change, now it become 10, how is it come from?
# 2 + 4 * (2/1) = 10