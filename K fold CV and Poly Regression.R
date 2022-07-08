# This script is from our professor's lecture
# This script is an example of using cross validation to find the best degree
# for polynomial regression

# function to generate data
sim_data <- function(n = 100){
  x   <- runif(n, -pi, pi)
  y   <- sin(x) + rnorm(n)
  out <- data.frame(x, y)
  return(out)
}

# function to compute k-fold CV MSE for polynomial regression
cv_mse <- function(data, d = 1, k = 5){
  
  # create folds randomly
  n <- nrow(data)
  folds <- sample(rep(1:k, length = n))
  
  # create vector to store results
  mse <- rep(NA, k)
  for(j in 1:k){
    
    # train model on all folds except j
    train <- folds != j
    ols  <- lm(y ~ poly(x, degree = d, raw = T), data = data[train, ])
    
    # compute MSE on fold j (not used for training)
    yhat <- predict(ols, newdata = data[!train, ])
    mse[j]  <- mean((data$y[!train] - yhat)^2)
  }
  # compute average mse
  mse.cv <- mean(mse)
  return(mse.cv)
}

# Example of 100 sample-----------------------------------------
set.seed(10)

# simulate sample of size n = 100
data.1e2 <- sim_data(n = 100)

# compute MSE's for d from 1 to 10
degree <- 1:10
cv.mse.1e2 <- sapply(degree, function(d) cv_mse(data.1e2, d))

best.1e2 <- degree[which.min(cv.mse.1e2)]
best.1e2

# fit model using best degree
ols <- lm(y ~ poly(x, degree = best.1e2, raw = T), data = data.1e2)

# predicted values
yhat <- predict(ols)

# plot against data and true CEF
plot(y ~ x, data.1e2, pch = 20)
curve(sin, col = "red", from = -pi, to = pi, add = T, lwd = 4)
lines(yhat[order(x)] ~ sort(x), data= data.1e2, col = "blue", lwd = 4)
legend("topleft", col = c("red", "blue"), legend = c("true", "fitted"), lty = 1, bty = "n", lwd = 4)

# Example of 1000 sample ---------------------------------------------

set.seed(10)

# simulate sample of size n = 1,000
data.1e3 <- sim_data(n = 1000)

# compute MSE's for d from 1 to 10
degree <- 1:10
cv.mse.1e3 <- sapply(degree, function(d) cv_mse(data.1e3, d))

# best degree
best.1e3 <- degree[which.min(cv.mse.1e3)]
best.1e3

# fit model using best degree
ols.1e3 <- lm(y ~ poly(x, degree = best.1e3, raw = T), data = data.1e3)

# predicted values
yhat.1e3 <- predict(ols.1e3)

# plot against data and true CEF
plot(y ~ x, data.1e3, pch= 20)
curve(sin, col = "red", from = -pi, to = pi, add = T, lwd = 4)
lines(yhat.1e3[order(x)] ~ sort(x), data= data.1e3, col = "blue", lwd = 4)
legend("topleft", col = c("red", "blue"), legend = c("true", "fitted"), lty = 1, bty = "n", lwd = 4)

# Example of 10000 samples ------------------------------------------------

set.seed(10)

# simulate sample of size n = 1,000
data.1e4 <- sim_data(n = 10000)

# compute MSE's for d from 1 to 10
degree <- 1:10
cv.mse.1e4 <- sapply(degree, function(d) cv_mse(data.1e4, d))

# best degree
best.1e4 <- degree[which.min(cv.mse.1e4)]
best.1e4

# fit model using best degree
ols.1e4 <- lm(y ~ poly(x, degree = best.1e4, raw = T), data = data.1e4)

# predicted values
yhat.1e4 <- predict(ols.1e4)

# plot against data and true CEF
plot(y ~ x, data.1e4, pch= 20)
curve(sin, col = "red", from = -pi, to = pi, add = T, lwd = 4)
lines(yhat.1e4[order(x)] ~ sort(x), data= data.1e4, col = "blue", lwd = 4)
legend("topleft", col = c("red", "blue"), legend = c("true", "fitted"), lty = 1, bty = "n", lwd = 4)

# Conslusion -----------------------

# We can observe that the more data we generate, the fitted line close to 
# the true line. Also, the best degree changes whenever the sample size change.
# Therefore, we should use cross validation when our data or sample size is different.