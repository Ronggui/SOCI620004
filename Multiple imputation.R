library(mice)
## missing at random or not?

md.pattern(Prestige)
## 4 cases have missing values (in variable of type)

Prestige$prestige[20] <- NA
md.pattern(Prestige)

