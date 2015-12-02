## confidence interval of median of prestige by non-parametric bootstrapping
data(Prestige, package="car")

library(boot)
median_stat <- function(x, index){
  median(x[index])
}

median_boot <- boot(Prestige$prestige, statistic = median_stat, R=1000)
boot.ci(median_boot, type = c("norm", "perc", "bca"))

hist(median_boot$t)
box()
abline(v=c(40.2, 50.2), col="red", lwd=2)
