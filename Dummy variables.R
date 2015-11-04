library(car)

## simmulate a dataset
x <- rnorm(mean=10, sd=5, n=500)
g <- factor(sample(x=letters[1:3], size=500,replace=TRUE))
m <- model.matrix(~x+g)
coef <- c(5, 0.2, -0.14, 0.13)
y <- m %*% coef + rnorm(n=500)
y <- as.vector(y)

## note g is a factor, you can see this by:  class(g)
mod <- lm(y ~ x + g)
## two coefficients associated with g
summary(mod)
## joint test of the two coefficients
Anova(mod)
