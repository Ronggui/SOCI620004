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
names(effs)
plot(effs, "education")

plot(effs, "income:type")
## change the title
plot(effs, "income:type", main ="Interaction effect")
## change xlab
plot(effs, "income:type", main ="interaction effect", xlab = "This is income")
plot(effs, "income:type", main ="interaction effect", xlab = "This is income", ylab="Occupational Prestige")
## fine tuning
foo <- plot(effs, "income:type", main ="interaction effect", xlab = "This is income", ylab="Occupational Prestige")
library(lattice)
dimnames(foo)
dimnames(foo)$type <- c("蓝领", "专业人士", "白领")
dimnames(foo)
foo$strip <- function(...) strip.default(..., strip.names = c(FALSE, TRUE))
print(foo)
## produce black and white plots
dev.off()
trellis.device(color=FALSE)
print(foo)

## compare coefficients in table-style
compareCoefs(m0, m1)
library(memisc)
mtable(m0, m1)

## expotr to APA-style table
library(apaTables)
apa.reg.table(m0, m1, filename="apaTable.doc")
  ## need to fully spell out the filename argument (bug?)
