## confidence interval of median of prestige by non-parametric bootstrapping
data(Prestige, package="car")

R = 10000
ans <- vector(mode="numeric", length=R)
for (i in 1:R){
  samp <- sample(Prestige$prestige, size = length(Prestige$prestige), replace = TRUE)
  ans[i] <- median(samp)
}
hist(ans)
quantile(ans, probs = c(0.025, 0.975))

library(boot)

median_stat <- function(x, index){
  dat <- x[index]
  median(dat)
}
median_boot <- boot(Prestige$prestige, statistic = median_stat, R=1000)
boot.ci(median_boot, type = c("norm", "perc", "bca"))

hist(median_boot$t)
box()
abline(v=c(40.2, 50.2), col="red", lwd=2)

median_stat2 <- function(x, index){
  dat <- x[index , ]
  median(dat$prestige)
}
median_boot2 <- boot(Prestige, statistic = median_stat2, R=1000)
boot.ci(median_boot2, type = c("norm", "perc", "bca"))
