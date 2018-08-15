library(nlme)
head(MathAchieve)
head(MathAchSchool)
MathAchieve$MEANSES <- MathAchSchool$MEANSES <- NULL

math <- merge(x = MathAchieve, y = MathAchSchool, by = "School", x.all = TRUE, all.y = FALSE)
dim(MathAchieve)
dim(math)

names(math) <- tolower(names(math))
math$meanses <- with(math, ave(ses, by=school, FUN = mean))
math$medianses <- with(math,ave(ses, by=school, FUN = median))

head(math)

# visualize variation of coef
mod_ols = lmList(mathach ~ ses|school, data=math)
summary(mod_ols)
plot(intervals(mod_ols))

# linear mixed effect models
mod = lme(fixed=mathach ~ 1, data=math, random = ~1|school)
summary(mod)
VarCorr(mod)

summary(lme(fixed=mathach ~ ses, data=math, random = ~1|school))

summary(lme(fixed=mathach ~ ses + sector, data=math, random = ~1|school))
summary(lme(fixed=mathach ~ ses*sector, data=math, random = ~1|school))

# random coefficiant model
summary(lme(fixed=mathach ~ ses, data=math, random = ~ses|school))

# three level model
data(Chem97, package="mlmRev")
summary(lme(fixed=score ~ gcsecnt, data=Chem97, random = ~1|lea/school)) # level3/level2

# further reading on nlme
# Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models in S and S-PLUS", Springer.

# Note: lme4 is the replacement of nlme

library(lme4)
summary(lmer(mathach ~ 1|school, data=math))
summary(lmer(mathach ~ ses + (1|school), data=math))
summary(lmer(mathach ~ ses + (1+ses|school), data=math))
summary(lmer(mathach ~ ses + (1+ses||school), data=math))

mod = lmer(mathach ~ ses*sector + (1|school), data=math)
confint(mod, "beta_") # use interval instead of pvalues
