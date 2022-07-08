# Frisch-Waugh-Lowell(FWL) Theorem
# this theorem is useful when we want to get the coefficient
# for only one factor. (multivariate -> Univariate)
# get the coefficient without other casual effect

# normal regression for y on x+z ------------------------------------------
# suppose we have these data for x and y
x <- rnorm(10000, 1, 1)  # the third parameter is SD, not Var.
z <- rnorm(10000, 3, 2)
e <- rnorm(10000, 0, 1)
beta_0 <- 2
beta_1 <- 2
beta_2 <- 4
y <- beta_0 + beta_1*x + beta_2*z + e
ols <- lm(y ~ x+z)
coef(ols)

# use FWL theorem to get the beta_2 ---------------------------------------
y_res <- y - predict(lm(y~x))
z_res <- z - predict(lm(z~x))

ols2 <- lm(y_res ~ z_res)
ols2
# check the coefficients of z_res, it's the same with beta_2

# how about we set the original y with x+z^2  -----------------------------
x <- rnorm(10000, 1, 1)  # the third parameter is SD, not Var.
z <- rnorm(10000, 3, 2)
e <- rnorm(10000, 0, 1)
beta_0 <- 2
beta_1 <- 2
beta_2 <- 4
y <- beta_0 + beta_1*x + beta_2*z^2 + e
ols <- lm(y ~ x+z)
coef(ols)
# we can find that the coefficient of z become 24
y_res <- y - predict(lm(y~x))
z_res <- z - predict(lm(z~x))

ols2 <- lm(y_res ~ z_res)
ols2
# FWL theorem still holds even if the model is not correctly specify.

# if we want to partial out the beta_1
y_res <- y - predict(lm(y~z))
x_res <- x - predict(lm(x~z))

ols2 <- lm(y_res ~ x_res)
ols2 # check the coefficient of x_res
