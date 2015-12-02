library(car)
m0 <- lm(prestige ~ income + education + type, Prestige)
m1 <- lm(prestige ~ income*type + education, Prestige)
summary(m1)
Anova(m0, m1)

## coefficent plots for presentation
library(coefplot)
coefplot(m0)
coefplot(m1)
multiplot(m0, m1)

## visualize the interaction effects
library(effects)
effs <- allEffects(m1)
plot(effs, "income:type")

## compare coefficients in table-style
compareCoefs(m0, m1)
library(memisc)
mtable(m0, m1)

## expotr to APA-style table
library(apaTables)
apa.reg.table(m0, m1, filename="apaTable.doc")
  ## need to fully spell out the filename argument (bug?)
