data(USArrests)
# correlation matrix
cor(USArrests)

# visualization
library(car)
scatterplotMatrix(USArrests)

# principal components analysis
ans = princomp(~ Murder + Assault + Rape, data = USArrests, cor = TRUE)

# choose number of principal components
summary(ans)
plot(ans)
# variable-obversation relationship
biplot(ans)

# combine principal scores with orginal dataset
newdata = cbind(USArrests,ans$scores)
