## Basics
1 + 2
3 - 2
2 * 6
6 / 3
2 ** 4
2 ^ 4
2 ^ 4 - 3 * 2
(4 + 3)^2
2 - -4

log(10)
log10(10)
log(10, base=10)
log(x=10, base=10)
log(10, 10)
exp(2)
log(exp(2))
sqrt(9)


args(log)
help(log)
?log

read.dta()
library(foreign) 
# load an installed package
install.packages("openxlsx")
library(openxlsx)
# install first and then load a package
install.packages("car")

getwd()
setwd("/Users/rghuang/Downloads/")
getwd()

curve(dnorm, -6, 6)
#curve(expr = dnorm, from = -6, to = 6)
curve(dnorm(x, sd=2), add=TRUE, col="blue", lty=2)
#curve(dnorm(x, mean=1), add=TRUE, col="red", lty=3)
curve(dnorm(x, 1), add=TRUE, col="red", lty=3)
abline(v=1, lty=3, col="red", lwd=4)
abline(v=0, lty=1, col="black", lwd=1)

?abline

curve(dnorm, -6, 6)
curve(dt(x, df=30), add=TRUE, col="red", lty=2, lwd=2)

ls()
1
ls()
a = 1
ls()
a = 1 + 8
b = 6 + 10

vec = c(1 , 2, 3)
vec2 = c(3 , 2, 1)
vec3 = 2:9
vec4 = seq(1, 10)
seq(1, 10, by=2)
seq(1, 10, by=0.5)
seq(-1, 10, by=0.5)
# class(vec)
vec + vec2
vec - vec2
vec * vec2 # not inner product
vec4 + vec

vec_str = c('初级统计' , "中级统计")
中级统计 = vec
vec_str = c('初级统计' , 中级统计)

matrix(c(1,2,3,4,5,6), nrow=2)
matrix(c(1,2,3,4,5,6), ncol=2)
m <- matrix(c(1,2,3,4), nrow=2, byrow=TRUE)
m + m
m - m
t(m)
m * m
m %*% m
solve(m)
m %*% solve(m)
round(m %*% solve(m), 10)
dim(m)
ncol(m)
nrow(m)

x = c(20, 21, 22)
y = c('F', 'F', 'M')
d = data.frame(age=x, gender=y)

?"$"
summary(d$age)

d$age
d$age > 20
subset(d, age>20)
subset(d, d$age>20)
d[d$age>20, ]
?subset


class(d$gender)
class(y)
class(d$age)
gender <- rep(c("male", "female"), each = 20)
gender
class(gender)
genderf = factor(gender)
class(genderf)
levels(genderf)
summary(gender)
summary(genderf)
?factor

factor( c("a ", "a", "b"))
f2 = factor( c("a", "a", "b", "c"))
factor(f2, levels=c("a", "b") )
factor(f2, levels=c("a", "b", "d") )
factor(c("男", "女", "男", "女"))
#factor(c("男", "女", "男"，"女"))


(ff <- factor(substring("statistics", 1:10, 1:10), levels = letters))
substring("statistics", 1:10, 1:10)
letters
factor(ff)
ff2 = factor(ff)
ff2
factor(ff2, levels=levels(ff2)[-1])
levels(ff2)[-1]
factor(ff2, levels=c('c', 'i', 's', 't'))
levels(ff2)
levels(ff2)[2:5]
levels(ff2)[-1]
levels(ff2)[-2]
levels(ff2)[c(-1, -2)]
ff2

ff3 = ff2
levels(ff3) = list("a"= c("a", "c"), "s"=c("s", "t"))
levels(ff3) = list("a"= c("a", "c"), "i" = c("i"), "s"=c("s", "t"))

library(car)
recode(ff2, "    'a'='a' ;  'c'='a' ;  's'='a'    ")
recode(ff2, "    'a'='a' ;  'c'='a' ;  's'='a' ; 't' = NA    ")

recode(ff2, " 'a'='a'; 'c'='a'; 'i'='i'; 's'= 'i'; 't'='i'")
# install.packages("car")
library(car)
recode(ff2, " 'a'='a'; 'c'='a'; 'i'='i'; 's'= 'i'; 't'='i'")
as.character(ff2)

library(foreign)
?read.spss

?read.dta
cgss2010 = read.dta("/Users/rghuang/Documents/CGSS2010/CGSS2010.dta")
warnings()
head(cgss2010)
table(cgss2010$sex)
class(cgss2010$sex)
levels(cgss2010$sex)
lll = levels(cgss2010$sex)
Sys.getlocale()
iconv(lll, from="gbk", to='utf-8')
lll2 = iconv(lll, from="gbk", to='utf-8')
levels(cgss2010$sex) = lll2
# d$str = iconv(d$str, "gbk", 'utf-8')
table(cgss2010$sex)
cgss2010 = read.dta("/Users/rghuang/Documents/CGSS2010/CGSS2010.dta")
levels(cgss2010$sex) = iconv(levels(cgss2010$sex), from="GBK", to="UTF-8")
table(cgss2010$sex)
cgss2010$sex_new = factor(cgss2010$sex, levels=c('男', '女'))
table(cgss2010$sex_new)
levels(cgss2010$sex)
cgss2010$sex_new = factor(cgss2010$sex, levels=c('男', '女'))

library(openxlsx)
?read.xlsx

?is.na
NA
