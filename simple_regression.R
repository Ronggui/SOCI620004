# load("HuangGui2014-SOCI620004.RData")
library(car)
scatterplot(nforward~fans, data=wb)
scatterplot(nforward~fans, data=wb, log="xy")

# fit simple regression model and assign it to srm1
srm1 <- lm(log(nforward)~log(fans), data=wb)
# get more details
summary(srm1)

