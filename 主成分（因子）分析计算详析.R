library(foreign)
bg2 <- read.dta("http://www.stata-press.com/data/r12/bg2.dta")

R = cor(bg2[, 2:4])

## 特征值分解
r = eigen(R)

## R V = lambda V
## demonstrate with the first eigen value
R %*% r$vectors[,1, drop=FALSE]
r$values[1] * r$vectors[,1, drop=FALSE]

R %*% r$vectors[,2, drop=FALSE]
r$values[2] * r$vectors[,2, drop=FALSE]

R %*% r$vectors[,3, drop=FALSE]
r$values[3] * r$vectors[,3, drop=FALSE]

R %*% r$vectors 
r$vectors %*% diag(r$values)

### 通过eigen进行主成分分析
pc = princomp(bg2[, 2:4])
summary(pc)
pcscores = predict(pc)
head( pcscores )

## 成分方差
round(cov(pcscores), 3)

sum(r$values)
r$values / sum(r$values)

## 手工计算主成分得分
head( as.matrix(bg2[, 2:4]) %*% r$vectors )

## 仅计算第一成分得分
# head( as.matrix(bg2[, 2:4]) %*% r$vectors[, 1, drop=FALSE] )

##### factor loading from prcomp (p.98)
## cov(f, f) = lambda
## cov(f, x) = lambda *v
## cor(f, x) = cov(f, x) / sqrt(sigma^2_f) = lambda * sqrt(v)

## demon cor
cor(pcscores[, 1], bg2[, 2])
r$vectors[1, 1] * sqrt(r$values[1])

### 在上述主成分分析中，直接对相关矩阵进行特征值分解
### 因子分析则是共因子模型，因子分析可以借助特征值分解进行计算
### 然而，分解的矩阵不是相关矩阵，而是剔除uniqueness的矩阵(S* or R*)
### 对于uniqueness或者共同度的估算有不同的计算方法，比如计算多元相关系数
### 详见：Everitt, B. An R and S-Plus Companion to Multivariate Analysis. Springer, 2005, Chapter 4.
