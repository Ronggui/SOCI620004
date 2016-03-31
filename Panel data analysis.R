library(plm)

data("Produc", package = "plm")

## pooling
pool.mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp*year, data = Produc, model = "pooling", index = c("state","year"))
summary(pool.mod)

##  first-differences
fd.mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "fd", index = c("state","year"))
summary(fd.mod)

## fixed effect
fixed.mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, model="within", data = Produc, index = c("state","year"))
summary(fixed.mod)

## randome effect
rand.mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, data = Produc, model = "random", index = c("state","year"))
summary(rand.mod)

## hausman test
phtest(fixed.mod, rand.mod)
## if significant, fixed effect model is preferred
