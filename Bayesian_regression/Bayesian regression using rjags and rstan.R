data(Duncan, package="car")
ols <- lm(prestige ~ income + education, data = Duncan)
summary(ols)

## stan use ; to indicate the end of a line (same as c++)
## use // to indicate comments (same as c++)
## need to specify data{} and parameters{}, as well as model{}
## support vecotrized computation

library(rstan)

scode <- "
data {
  int<lower=0> N; // number of obs
  int<lower=0> K; // number of IVs
  matrix[N,K] x; // design matrix; x is a matrix (not array)
  vector[N] y; // DV
}
parameters {
  vector[K] beta; // coef
  real<lower=0> sigma;
}
model{
  y ~ normal(x * beta, sigma); // vectorized computation
  beta[1] ~ normal(0, 100); // priors
  beta[2] ~ normal(0, 100);
  beta[3] ~ normal(0, 100);
  sigma ~ uniform(0, 100);
}
"


fit <- stan(model_code=scode, thin = 100, iter = 5000,
            data=list(N=nrow(Duncan), K=3, x=model.matrix(ols), y=model.response(ols$model)))
fit
traceplot(fit)


## jags does not support vecotrized computation and needs to loop over the data points
## ; is not needed at the end of the lines
## use # to indicate comments
## the names of functions are different:
   ### dnorm vs normal [dnorm use precision instead of sd as the second argument]
   ### dunif vs uniform

library(rjags)

jcode <- "
model {
  for (i in 1:N){
    y[i] ~ dnorm(y.hat[i], tau)
    y.hat[i] <- beta1 + beta2*income[i] + beta3*education[i]
  }
  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 100)
}
"

jags <- jags.model(textConnection(jcode), data=list(N=nrow(Duncan), 
                                    y=Duncan$prestige,
                                    income=Duncan$income,
                                    education=Duncan$education),
                   n.chains=4, n.adapt=100)

update(jags, 1000)
samps <- coda.samples(jags, c("beta1", "beta2", "beta3", "sigma"), 1000)
summary(samps)


## OLS model with an interaction term
ols_2 <- lm(prestige ~ income * education, data = Duncan)
coef(ols_2)

jcode_2 <- "
model {
  for (i in 1:N){
    y[i] ~ dnorm(y.hat[i], tau)
    y.hat[i] <- beta1 + beta2*income[i] + beta3*education[i] + beta4*income[i]*education[i]
  }
  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  beta4 ~ dnorm(0, 0.0001)
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 100)
}
"

jags_2 <- jags.model(textConnection(jcode_2), data=list(N=nrow(Duncan), 
                                    y=Duncan$prestige,
                                    income=Duncan$income,
                                    education=Duncan$education),
                   n.chains=4, n.adapt=100)

update(jags_2, 1000)
samps_2 <- coda.samples(jags_2, c("beta1", "beta2", "beta3", "beta4", "sigma"), 1000)
summary(samps_2)


