library(car)

## leverage
Prestige10 <- Prestige[1:10,]
m <- lm(prestige ~ income + women, data = Prestige10)
hatvalues(m)

## change the size of symbols by cex
plot(fitted(m), rstudent(m), cex = hatvalues(m) * 3)
plot(fitted(m), rstudent(m), cex = cooks.distance(m) * 2)

## demonstrate the calculation of hatvalues
X <- model.matrix(m)
H <- X %*% solve(crossprod(X)) %*% t(X)
H <- X %*% solve( t(X) %*% X ) %*% t(X)

all.equal(diag(H), hatvalues(m))

hvs <- apply(H, 1, function(x) sum(x^2))
all.equal(hvs, hatvalues(m))

all.equal( mean(hatvalues(m)), m$rank / (m$df.residual + m$rank) )

hatvalues(m)

## multimodal residuals and potential causes (e.g. missing categorical variable)
x <- rnorm(n = 300, mean = 50, sd = 5)
g <- rbinom(n = 300, prob = 0.4, size = 1)
z <- rnorm(n = 300, mean = 30, sd = 8)
y <- 2 + 0.7 * x + 4 * g + rnorm(n=300)
hist( residuals(lm(y ~ x)) )
box()
hist(residuals(lm(y ~ x + g)))
box()
qqPlot(rstudent(lm(y ~ x + g)))

## normal distribution of residuals
m = lm(interlocks ~ sector + assets, data = Ornstein)
qqPlot( rstudent( m ) )

m1 <-  lm(interlocks ~ sector + assets, data = Ornstein)
rst <- rstudent(m1)
qqPlot(rst)

set.seed(5000)
x <- rnorm(n = 1000, mean = 80, sd=6)
z <- rnorm(n = 1000, mean = 10, sd=3)
y <- 2 + 3 * log10(x) + 0.7 * z + rnorm(n = 1000)
# x <- x - 70 ; y <- 2 + 3*x+5*x^2+0.7*z + rnorm(n=1000)
m <- lm(y~x+z)

m <- lm(prestige ~ income + education, data = Prestige)
crPlots(m)
crPlot(m, "income")

avPlots(m)
avPlot(m, "income")

plot(m)

residuals(m)
rstudent(m)
rstandard(m)

cooks.distance(m)

dfbeta(m)
dfbetas(m)
dffits(m)

# Non-Constant Error Variance / heteroskedasticity
ncvTest(m)
library(lmtest)
bptest(m)
## robust SE
coeftest(m, vcov=hccm)
coeftest(m, vcov=hccm(m))

## test lack of fit/linearity
Vocab <- subset(Vocab, year==1989)
m1 <- lm(vocabulary ~ education, data=Vocab)
m2 <- lm(vocabulary ~ factor(education), data=Vocab)
anova(m1, m2)


vif(m)

HH::vif(m)

perturb::colldiag(m) ## condition number

## explain differences between Type-II  and Type-III test
m1 <- lm(prestige ~ income + type, data = Prestige)
Anova(m1, type="II") ## principle of marginality

m2 <- lm(prestige ~ income*type, data = Prestige)
Anova(m2, type="II") ## principle of marginality
Anova(m2, type="III") ## violate principle of marginality

## Anova not anova
