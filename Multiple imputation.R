################
# use mi packge to do multiple imputations
################
library(mi)
### load a dataset into R
data(nlsyV, package = "mi")
## convert it to an enchanced_data_classs called missing_data.frame
mdf <- missing_data.frame(nlsyV)
## show details about missing patterns
show(mdf)
## change variable types to approriate ones
mdf <- change(mdf, y = c("income", "momrace"), what = "type", to = c("non", "un")) ## un = unordered-categorical; non = nonnegative-continuous
## use class?missing_variable to see more details
show(mdf) ## check again

summary(mdf)
image(mdf)
hist(mdf)

## multiple imputation
imputations <- mi(mdf, n.iter = 30, n.chains = 4, max.minutes = 20) ## 迭代次数； 链数； 最长运算时间
show(imputations)

## 如果迭代次数足够大，那么运算收敛，每个运算链结果类似，否则不收敛。可以检查每个运算链得到结果的均值是否类似。
round( mipply(imputations, mean, to.matrix = TRUE), 3)

## if not converged, keep going
imputations <- mi(imputations, n.iter = 5)

## can use visualization to examine the result
# plot(imputations, y=c("ppvtr.36"))

## modeling
mod <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed + momrace, data = imputations, m = 5) ## use 5 imputed datasets
summary(mod) 
display(mod) ## bayesglm

## for more information, see > vignette("mi_vignette", package="mi")


################
# use mice packge to do multiple imputations
################
library(mice)

print(nhanes)
## examining missing patterns
md.pattern(nhanes)
p <- md.pairs(nhanes)
p

## multiple imputations
imp <- mice(nhanes, seed = 23109)
print(imp)

## diagnostic analysis - skipped

## modeling
fit <- with(imp, lm(chl ~ age + bmi))
print(fit) ## one model per imputation
pool(fit) ## pool to form one solution
summary(pool(fit)) ## lambda is the proportion of the total variance that is attributable to the missing data

## increase the number of imputations
imp50 <- mice(nhanes, m = 50, seed = 23109)
fit50 <- with(imp50, lm(chl ~ age + bmi))
summary(pool(fit50)) 

## choose approriate models to imputate different types of variables
imp_meth <- mice(nhanes, method = c("", "norm", "pmm", "mean"))
## for more information, see 
## Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/
