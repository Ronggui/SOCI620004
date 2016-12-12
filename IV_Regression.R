# install.packages('AER')
library(AER)

## data and variables
　# tax： Average state, federal and average local excise taxes for fiscal year.
  # taxs： Average excise taxes for fiscal year, including sales tax.

data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi) ## real price
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi) ## real income per person
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi) ## real tax on cigarettes arising from the state’s general sales tax
## only analyze on a single year
Cigarettes95 = subset(CigarettesSW, year == "1995")

### 结构模型
## 香烟消费量在多大程度上受价格的影响；控制变量为人均收入
## 可能内生的变量：价格； 因为价格上升将导致消费需求下降，反之上升
### 工具变量
## 税收：税收由政府决定，而不是市场决定
## 模型设定：对变量作对数变换

## model
iv1 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff, data = Cigarettes95)
summary(iv1)
## diagnostic test 
## an F test of the first stage regression for weak instruments
## a Wu-Hausman test for endogeneity
## a Sargan test of overidentifying restrictions (if more instruments than regressors)
summary(iv1, diagnostics = TRUE)

## more IVs
iv2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi), data = Cigarettes95)
summary(iv2, diagnostics = TRUE)

# Diagnostic tests:
#   df1 df2 statistic p-value    
#   Weak instruments   2  44   244.734  <2e-16 *** ## significance indicates that IVs are not weak
#   Wu-Hausman         1  44     3.068  0.0868 .   ## significance indicates endogeneity 
#   Sargan             1  NA     0.333  0.5641     ## not significant, then IVs are not correlated with errors

