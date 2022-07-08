rm(list = ls())

# Load packages
library(dagitty)
library(ggdag)

# simulation of common effects
n <- 400
beauty <- rnorm(n)
intelligence <- rnorm(n)

plot(beauty ~ intelligence, pch = 20)

m <- lm(beauty ~ intelligence)
summary(m) # there is almost no association on it, because coefficient for "intelligence" is almost 0.

# seperate if you want to date or not
# suppose our condition is beauty + intelligence bigger than 0
dating <- (beauty + intelligence > 0)
plot(beauty ~ intelligence, col = ifelse(dating, "red", "blue"), pch = 20)

# if we condition on we dating or not
# the coefficient become a non-casual association
m1 <- lm(beauty ~ intelligence, subset = dating == 1)
summary(m1) # we can see that now the coefficient of "intelligence" changed.

m0 <- lm(beauty ~ intelligence, subset = dating == 0)
summary(m0)

# model 1

# the plot for model 1
model <- dagitty("dag{d->y; x->y; x->d}")

coordinates(model) <-  list(
  x = c(d=1, y=3, x=2),
  y = c(d=1, y=1, x=2))

ggdag(model) + theme_dag()

n <- 1000
x <- rnorm(n)
d <- x + rnorm(n)
y <- x + d + rnorm(n)

lm(y ~ d)  # we found coefficient is 1.49, it should be 1 if there is no confounder.

lm(y ~ d + x) # if you adjust for x, you get what you want.

# model 2

# The plot for model 2
# specify edges
model <- dagitty("dag{d->y; u->d; u->x; x->y}")

# set u as latent
latents(model) <- "u"

## coordinates for plotting
coordinates(model) <-  list(
  x = c(d=1, x=3, u=2, y = 4),
  y = c(d=1, y=1, x=2, u = 3))

## ggplot
ggdag(model) + theme_dag()


n <- 100
u <- rnorm(n) # unobserved
d <- u + rnorm(n)
x <- u + rnorm(n)
y <- x + d + rnorm(n)

lm(y ~ d)
lm(y ~ d + u) # but u is unobserved
lm(y ~ d + x)

# model 3
model <- dagitty("dag{d->y; u->x; x->d; u->y}")

# set u as latent
latents(model) <- "u"

## coordinates for plotting
coordinates(model) <-  list(
  x = c(d=1, x=2, u=3, y = 4),
  y = c(d=1, y=1, x=2, u = 3))

## ggplot
ggdag(model) + theme_dag()

n <- 10000
u <- rnorm(n) # unobserved
x <- u + rnorm(n)
d <- x + rnorm(n)
y <- d + u + rnorm(n)

lm(y ~ d)
lm(y ~ d + u) # but u is unobserved
lm(y ~ d + x)

# model 7

model <- dagitty("dag{d->y; u1->d; u1->x; u2->x; u2->y}")

# set u as latent
latents(model) <- c("u1", "u2")

## coordinates for plotting
coordinates(model) <-  list(
  x = c(d=1, u1=1, x=2, u2=3, y=3),
  y = c(d=1, u1=2, x=2, u2=2, y=1))

## ggplot
ggdag(model) + theme_dag()

n <- 100000

Udx <- rnorm(n)
Uxy <- rnorm(n)
x <- Udx + Uxy + rnorm(n)
d <- Udx + rnorm(n)
y <- Uxy + d + rnorm(n)

lm(y ~ d)
lm(y ~ d + x)  # you block x will cause biases.

# what if there is no d
Udx <- rnorm(n)
Uxy <- rnorm(n)
x <- Udx + Uxy + rnorm(n)
d <- Udx + rnorm(n)
y <- Uxy + 0*d + rnorm(n)

lm(y ~ d)      # no causal effect
lm(y ~ d + x)  # negative effect.
