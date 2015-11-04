load("HuangGui2014-SOCI620004.RData")
## reconstruct the categorical variable of zhuce (1; 2; 3)
wb$zhuce <- 1
wb$zhuce[wb$weizhuce==1] <- 2
wb$zhuce[wb$gongshang==1] <- 3

## convert to factor
wb$zhuce <- factor(wb$zhuce, labels=c("ngo", "weizhuce", "gongshang"))
wb$v <- factor(wb$v)

summary(lm(log(nforward)~yrs+wbage+v+zhuce,data=wb))
mod2 <- lm(log(nforward)~yrs+wbage+v*zhuce,data=wb)
summary(mod2)

library(effects)
eff <- allEffects(mod2, xlevels=list(v=c(0,1)))
names(eff)
plot(eff["v:zhuce"])
# plot(eff["v:zhuce"], x.var="zhuce")

mod.r <- lm(log(nforward)~1+yrs+wbage+zhuce+v,data=wb)
mod.f <- lm(log(nforward)~1+(yrs+wbage+zhuce)*v,data=wb)
anova(mod.r, mod.f)

