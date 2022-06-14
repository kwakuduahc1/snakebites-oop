model <- function()
{
  beta ~ dnorm(0.000000001, 0.001)
  alpha ~ dnorm(0.00000001, 0.001)
  for (i in 1:n) {
    mu[i] <- alpha + beta * age[i]
    y[i] ~ dnorm(mu[i], 0.0001)
  }
}

data <- list(age = unlist(as.numeric(bites$age)), 
             #los = unlist(as.numeric(bites$los)), 
             n = as.integer(nrow(bites)))
params <- c("beta", "alpha", "y", "mu")

inits <- function() {
  list(beta = 0, alpha = 8)
}

model.file <- file.path(getwd(), "model.txt")
write.model(model, model.file) 
out <- bugs(data, inits, params, model.file, n.iter=100, n.chains = 1, debug = T)
all(out$summary[,"Rhat"] < 1.1)
# fitting the model 
cbind(unlist(out$mean[ c("mu", "beta")]))
# credible intervals 
out$summary[c("beta", "alpha"), c("2.5%", "50%", "97.5%")]
mean(out$mean$mu)
