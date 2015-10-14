example(anscombe)

library(car)
## barplot
barplot(table(Prestige$type),main="Bar Plot")
box()

## histogram
par(mfrow=c(2,1))
hist(Prestige$income, breaks=c(seq(0,30000,1000)), main="start at 0") ##figure 3.2a
hist(Prestige$income, breaks=c(seq(500,30000,1000)), main="start at 500") ##figure 3.2b
library(MASS)
truehist(Prestige$income, nbins="FD")

##use auto.dta
auto <- read.dta("http://www.stata-press.com/data/r9/auto.dta")
par(mfrow=c(1,2))
hist(auto$length,breaks=5, main="5 bins")
hist(auto$length,breaks=20, main="20 bins")

stem(Prestige$income)
## 茎1的叶接在茎0所有叶子的后面，茎3的叶接在茎2所有叶子的后面，以此类推
stem(Prestige$income, scale=2)

plot(density(Prestige$income),main="density plot of income")

hist(Prestige$income, breaks=17, prob=T, ylim=c(0,1.35e-4), main="Hist vs. density") ## use nclass.FD
lines(density(Prestige$income))

set.seed(1000)
plot.ecdf(rnorm(80),verticals=T,cex=0.5, main="ECDF of a normal distribution sample")
## 不光滑
curve(pnorm, add=T, col="red")

set.seed(100)
qqPlot(rnorm(100, mean=50, sd=10))

set.seed(100)
qqPlot(rchisq(100, df=2))
x=seq(-4,4,by=0.01)
y1=dnorm(x)
y2=dchisq(x,df=2)
plot(y2~x, type="l")
lines(x, y1, col="red")
legend(2, 0.45, c("chisq","norm"), col=c("black","red"), lty=1)

set.seed(50);qqPlot(rnorm(500, mean=30, sd=10), distribution="t", df=3)

set.seed(17); qqPlot(rt(100, df=2))
x=seq(-4,4,by=0.01)
y1=dnorm(x)
y2=dt(x,df=2)
plot(y1~x, type="l")
lines(x, y2, col="red")
legend(2, 0.38, c("t","norm"), col=c("red","black"), lty=1)

Boxplot(Prestige$income)
Boxplot(income ~ type, data=Prestige, main="Comparison of Income")
Boxplot(interlocks~nation,Ornstein)

data(Titanic,package="effects")
barplot(with(Titanic,table(survived,passengerClass)),beside=T,legend=T, xlab="Class", ylab="Frequency")
box()

with(Vocab,plot(vocabulary,education))
with(Vocab,plot(jitter(vocabulary,amount=0.5),jitter(education,amount=0.5)))

scatterplot(prestige~income, data=Prestige)
scatterplot(infant.mortality~gdp, data=UN)
scatterplot(log(infant.mortality)~log(gdp), data=UN)

scatterplot(prestige~income|type, data=Prestige)
scatterplotMatrix(~ income + education + prestige, transform=FALSE, data=Duncan, id.n=3)

library(rgl)
with(Prestige,plot3d(income, education, prestige))
rgl.snapshot("plot3d.png")
