library(car)

load("HuangGui2014-SOCI620004.RData")

## Visualize the data before modelling
scatterplot(nforward~fans, data=wb
scatterplot(nforward~fans, data=wb, log="xy")

## fit simple regression model and assign it to srm1
srm1 <- lm(log(nforward)~log(fans), data=wb)
summary(srm1)

## Joint test of coef
## create a categorical variable of zhuce (1; 2; 3)
wb$zhuce <- 1
wb$zhuce[wb$weizhuce==1] <- 2
wb$zhuce[wb$gongshang==1] <- 3

## test equality of two coefficients
## Frst manually create the dummy variables
wb$weizhuce <- recode(wb$zhuce, "2=1; NA=NA; else=0")
wb$gongshang <- recode(wb$zhuce, "3=1; NA=NA; else=0")

## multiple regression
mod <- lm(log(nforward)~yrs+wbage+v+weizhuce+gongshang,data=wb)
summary(mod)

# equality of two coef
linearHypothesis(mod, "weizhuce - gongshang = 0")
# joint test of weizhuce==gongshang=0
linearHypothesis(mod, c("weizhuce=0", "gongshang=0"))

## use Prestige example to demonstrate the calculation
mod.duncan <- lm(prestige ~ income + education, data=Duncan)
vcov(mod.duncan)
coef(mod.duncan)
## H0: coef_income == coef_edu
SED <- sqrt(0.014320275 + 0.009653582 + 2 * 0.008518551)
T <- (0.5987328-0.5458339) / SED
DF <- 42
pf(T, df = DF, lower = F) * 2
pf(T^2, df1 = 1, df2 = DF, lower = FALSE) ## use F test / wald test

## use linearHypothesis from car package
linearHypothesis(mod.duncan, "1 * income - 1 * education = 0")
## H0: coef_income = coef_edu = 0
linearHypothesis(mod.duncan, "income = education ")
