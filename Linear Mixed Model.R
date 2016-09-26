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
