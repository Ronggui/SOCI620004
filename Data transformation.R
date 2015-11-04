library(ggplot2)
library(gridExtra)

x <- 1:6

grid.arrange(
qplot(x,x^-1)+ geom_line(),
qplot(x,x^-1/-1)+ geom_line(),
nrow=1
)

x <- seq(0,6, by=0.2)
plot(x,(x^-1 -1 )/-1, type="l", ylim=c(-4,6))
lines(x,(x^-2 -1)/-2)
lines(x,(x^1 -1)/1)
lines(x,(x^2 -1)/2)
lines(x,(x^3 -1)/3)

x <- seq(1991,1996)
plot(x,log10(x),type="b", ylim=c(0,3.5),ylab="Transformed values")
lines(x,log10(x-1990),type="b")
text(locator(n=2),c("x, ratior=1.003","x-1990, ratio=6"))

ratio <- function(x) {ans <- (quantile(x, 0.75) - median(x)) / (median(x) - quantile(x, 0.25)); names(ans) <- "ratio"; ans}

par(mfrow=c(3,1))
x <- rnorm(n=800, mean=200, sd=80)
x <- x[x>0]
x <- x^0.33
plot(density(x), main="Negative skew")
xT <- bcPower(x2, lambda=3); ratio(xT)
plot(density(bcPower(x2, lambda=3)), main="Power of 3")

library(car)
par(mfrow=c(2,1))
plot(density(Prestige$income,from=100, to=31622.78),main="Original Variable", xlab="")
rug(Prestige$income)
xT <- log10(Prestige$income)
d <- density(xT, from=2, to=5)
plot(d, main="Transformed Variable",axes=F, xlab="")
axis(2)
axis(1, at=c(2,3,4,4.5), label=c("100", "1000","10000","31622"))
rug(xT)
box()

par(mfrow=c(1,3))
x <- rnorm(20, mean=2); x <- sort(x)
y <- 0.5 * x^2
plot(x,y, type="b", main=expression("y vs x"))
yn <- sqrt(y)
xn <- x^2
plot(x, yn, type="b", main=expression(paste(sqrt(y)," vs x")))
plot(xn, y, type="b", main=expression(paste("y vs ", x^2)))


par(mfrow=c(1,3))
x <- rnorm(20, mean=3); x <- sort(x)
y <- 0.5 * x^2
plot(x,y, type="b")
x2 <- rnorm(20); x2 <- sort(x2)
y2 <- 0.5*x2 + 1*x2^2
plot(x2,y2, type="b", main="")
##l <- locator()
plot(l$x, l$y)
lines(lowess(l$x, l$y,f=1/3))

plot(0,0,xlim=c(-1.5,1.5), ylim=c(-1.5,1.5),axes=F,xlab="",ylab="",cex=30)
abline(v=0,h=0)
text(locator(4),adj=c(0,0),
c(
expression(paste(y^2,", ",y^3)),
expression(paste(log(x),", ",sqrt(x))),
expression(paste(log(y),", ",sqrt(y))),
expression(paste(x^2,", ",x^3))
     )
)

par(mfrow=c(1,2))
plot(Prestige$income, Prestige$prestige)
lines(lowess(Prestige$income, Prestige$prestige, f=1/2),lty=2)
abline((lm(Prestige$prestige~Prestige$income)))
plot(Prestige$income^(1/3), Prestige$prestige)
abline((lm(Prestige$prestige~I(Prestige$income^(1/3)))))
lines(lowess(Prestige$income^(1/3), Prestige$prestige), lty=2)

UN <- subset(UN, !is.na(UN$gdp) & !is.na(UN$infant.mortality))
par(mfrow=c(1,2))
plot(UN$gdp,UN$infant.mortality)
lines(lowess(UN$gdp,UN$infant.mortality))
plot(log(UN$gdp),log(UN$infant.mortality))
abline(lm(log(UN$infant.mortality)~log(UN$gdp)))

par(mfrow=c(1,3))
boxplot(Ornstein$interlocks~Ornstein$natio, main="interlocks")
m <- log10(tapply(Ornstein$interlocks+1,Ornstein$nation,FUN=median))
s <- log10(tapply(Ornstein$interlocks+1,Ornstein$nation,FUN=IQR))
plot(m,s, main="Choice of power")
abline(lm(s~m))
text(0.8,1.3, "log spread = 0.37 + 0.80 * log median", adj=c(0,0))
text(0.8,1.28, expression(paste("power = 1 - b =", 0.2 %=~%0, collapse="")), adj=c(0,0))
boxplot(logb(Ornstein$interlocks+1)~Ornstein$natio, mai="logb(interlocks+1)")
