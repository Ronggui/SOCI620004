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


Prestige2 <- Prestige[!is.na(Prestige$type), ]

## two group chow test
ols1 <- lm(prestige ~ income + education, data = Prestige2)
summary(lm(prestige ~ income + education + income * type , data = Prestige2))

ols2 <- lm(prestige ~ (income + education) * I(type == "bc"), data = Prestige2)

anova(ols1, ols2)

## manually calculate the chow test for two groups
ols_1 <- lm(prestige ~ income + education, data = Prestige2, subset = type=="bc" )
ols_2 <- lm(prestige ~ income + education, data = Prestige2, subset = type!="bc" )
F <- (sum(ols1$residuals^2) - (sum(ols_1$residuals^2) + sum(ols_2$residuals^2))) /
      (sum(ols_1$residuals^2) + sum(ols_2$residuals^2)) *
      (ols1$df.residual - 3) / 3
F