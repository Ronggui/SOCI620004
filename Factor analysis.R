library(foreign)
bg2 <- read.dta("http://www.stata-press.com/data/r12/bg2.dta")

# factors=1
factanal(~bg2cost1+bg2cost2+bg2cost3+bg2cost4+bg2cost5+bg2cost6, factors=1, data=bg2)

# factors=2 is better
factanal(~bg2cost1+bg2cost2+bg2cost3+bg2cost4+bg2cost5+bg2cost6, factors=2, data=bg2)

# compute simple additive factor score
bg2$Factor1 <- bg2$bg2cost2 +  bg2$bg2cost3 + bg2$bg2cost4

# compute additive factor score using the loadings as weights
bg2$Factor1 <- 0.459*bg2$bg2cost2 +  0.721*bg2$bg2cost3 + 0.357*bg2$bg2cost4

# compute factor scores using regression method
fam2 <- factanal(~bg2cost1+bg2cost2+bg2cost3+bg2cost4+bg2cost5+bg2cost6, factors=2, data=bg2, scores="regression")
print(fam2, cutoff=0)

# inspect the factor scores, which is a matrix
head(fam2$scores) 
bg2$F1 <- fam2$scores[,1]
bg2$F2 <- fam2$scores[,2]
# You can use F1 and F2 in the ensuing analyses


library(psych)
# use the method of "principal axes"
fam2pa <- fa(bg2[,c("bg2cost1","bg2cost2","bg2cost3","bg2cost4","bg2cost5","bg2cost6")], nfactors=2, rotate="varimax", fm="pa")
bg2$F1PA <- fam2pa$scores[,1]
bg2$F2PA <- fam2pa$scores[,2]

# use the method of "minres"
fam2min <- fa(bg2[,c("bg2cost1","bg2cost2","bg2cost3","bg2cost4","bg2cost5","bg2cost6")], nfactors=2, rotate="varimax", fm="minres")

# number of factors
fa.parallel(bg2[,c("bg2cost1","bg2cost2","bg2cost3","bg2cost4","bg2cost5","bg2cost6")])
   
# reliability analysis
library(ltm)
cronbach.alpha(bg2[,c("bg2cost1","bg2cost2","bg2cost3","bg2cost4","bg2cost5","bg2cost6")])
cronbach.alpha(bg2[,c("bg2cost1","bg2cost2","bg2cost3","bg2cost4","bg2cost5","bg2cost6")], standardized = TRUE)
cronbach.alpha(bg2[,c("bg2cost1", "bg2cost5","bg2cost6")])


# demonstrate the relationships among various outputs
fans2= fa(bfi[1:25], nfactors=2)
fans2$loadings
apply(fans2$loadings, 1, function(x) sum(x^2))  # communality
apply(fans2$loadings, 1, function(x) 1-sum(x^2))  # unique variance
apply(fans2$loadings, 2, function(x) sum(x^2))  # SS loadings / factor contributions
(total_variance <- sum(apply(fans2$loadings, 1, function(x) 1-sum(x^2))) + sum(apply(fans2$loadings, 2, function(x) sum(x^2))))
apply(fans2$loadings, 2, function(x) sum(x^2))  / total_variance # Proportion Var
