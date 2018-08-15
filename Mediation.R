# simple mediation and moderation
library(medmod)

set.seed(1234)
X <- rnorm(50)
M <- 0.5*X + rnorm(500)
Y <- 0.7*M + rnorm(500)
dat <- data.frame(X=X, M=M, Y=Y)

med(dat, dep = "Y", pred = "X", med = "M") # standard method
med(dat, dep = "Y", pred = "X", med = "M", paths=TRUE) # show path estimates
med(dat, dep = "Y", pred = "X", med = "M", estPlot=TRUE) # plot effects
med(dat, dep = "Y", pred = "X", med = "M", paths=TRUE, estPlot=TRUE) # show paths and plot effects

med(dat, dep = "Y", pred = "X", med = "M", estMethod = "bootstrap") # use bootstrap method
med(dat, dep = "Y", pred = "X", med = "M", estMethod = "bootstrap", bootstrap = 2000) # number of bootstrap, large number is preferred
med(dat, dep = "Y", pred = "X", med = "M", estMethod = "bootstrap", bootstrap = 2000, ci = TRUE) # show confidence interval

# simple moderation
set.seed(1234)
X <- rnorm(10)
M <- rnorm(10)
X_M <- X*M
Y <- 0.7*X + 0.1*M + 4.2*X_M + rnorm(10)
dat <- data.frame(X=X, M=M, Y=Y)   

mod(dat, dep = "Y", pred = "X", mod = "M")
mod(dat, dep = "Y", pred = "X", mod = "M", estMethod = "bootstrap", ci=TRUE)

## related packages: RMediation


# causal mediation analysis
library(mediation)
####################################################
#  Linear Outcome and Mediator Models
####################################################
data(jobs)
fit.med <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
fit.y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
contcont <- mediate(fit.med, fit.y, sims=500, treat="treat", mediator="job_seek")
summary(contcont) 
# ACME: average causal mediation effect
# ADE: average direct effect 
plot(contcont)

contcont_boot <- mediate(fit.med, fit.y, sims=1000, treat="treat", mediator="job_seek", boot=TRUE)
summary(contcont_boot)

####################################################
#  Linear Outcome and binary mediator Models
####################################################
med.fit <- lm(emo ~ treat + age + educ + gender + income, data = framing)
out.fit <- glm(cong_mesg ~ emo + treat + age + educ + gender + income,data = framing, family = binomial("probit"))
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", robustSE = TRUE, sims = 100)
summary(med.out)


