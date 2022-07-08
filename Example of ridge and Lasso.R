rm(list = ls())

library(glmnet)

df <- read.csv("SaratogaHouses.csv")

X <-  model.matrix(price ~ . -1, data = df) # create dummy variables for categorical data
# "-1" means not include the constant
y <- df$price

# ridge part
ridge <- glmnet(X, y, alpha = 0)  # alpha = 0 means "ridge"
                                  # alpha = 1 means "Lasso"
plot(ridge)
plot(ridge, xvar = "lambda", label = T) # change the x axis
# label = T denotes the order of the variables
# coefficients will never shrink to 0.
cv.glmnet(X, y) # The way how you pick lambda

cv.ridge <- cv.glmnet(X, y, type = "mse", alpha = 0) # use the MSE to measure the performance
cv.ridge

# Lasso part
lasso <- glmnet(X, y, alpha = 1)
plot(lasso, xvar = "lambda", label = T)  # coefficients might shrink to 0
cv.lasso <- cv.glmnet(X, y, type = "mse", alpha = 1) 
cv.lasso

coef(cv.lasso)  # give you the "lse" as default
coef(cv.lasso, s = "lambda.min")  # get the coefficient as "min"
predict(cv.lasso, newx = X, s = "lambda.min")
