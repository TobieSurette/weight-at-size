library(gulf.data)
library(nimble)
library(gulf.graphics)

# Basic allometric model:
code <- nimbleCode({
   # Priors
   log.alpha ~ dnorm(0, sd = 100)
   beta ~ dnorm(3.0, 1);
   sigma ~ dunif(0, 100);
   
   # Model
   for (i in 1:n) {
      mu[i] <- log.alpha + beta * log(x[i]);
      y[i] ~ dlnorm(mu[i], sd = sigma);
   }
})

# Allometric model - with errors in observed sizes.
code <- nimbleCode({
   # Priors
   log.alpha ~ dnorm(0, sd = 100)
   beta ~ dnorm(3.0, 1);
   sigma ~ dunif(0, 100);
   
   # Model
   for (i in 1:n) {
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)  
      mu[i] <- log.alpha + beta * log(xt[i]);
      y[i] ~ dlnorm(mu[i], sd = sigma);
   }
})

# Allometric model with:
#    - errors in observed sizes.
#    - outlier error term.
code <- nimbleCode({
   # Allometric parameter priors:
   log.alpha ~ dnorm(0, sd = 100)      # Intercept parameter.
   beta ~ dnorm(3.0, 1);               # Slope parameter.
   
   # Error process priors:
   sigma[1] ~ dunif(0, 100);           # Observation error.
   delta.sigma ~ dunif(0, 100);
   sigma[2] <- sigma[1] + delta.sigma; # Outlier error.
   
   # Prior over outlier probabilities:
   P[1:2] ~ ddirich(alpha[])
   
   # Model definition:
   for (i in 1:n) {
      T[i] ~ dcat(P[])                        # Outlier indicator variable.
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)   # True fish size.
      mu[i] <- log.alpha + beta * log(xt[i]); # Mean weight prediction.
      sigma.obs[i] <- sigma[T[i]]   
      y[i] ~ dlnorm(mu[i], sd = sigma.obs[i]);
   }
})

# Allometric model with:
#    - errors in observed sizes.
#    - outlier error term.
#    - weight precision taken into account.
code <- nimbleCode({
   # Allometric parameter priors:
   log.alpha ~ dnorm(0, sd = 100)      # Intercept parameter.
   beta ~ dnorm(3.0, 1);               # Slope parameter.
   
   # Error process priors:
   sigma[1] ~ dunif(0, 100);           # Observation error.
   delta.sigma ~ dunif(0, 100);
   sigma[2] <- sigma[1] + delta.sigma; # Outlier error.
   
   # Prior over outlier probabilities:
   P[1:2] ~ ddirich(alpha[])
   
   # Model definition:
   for (i in 1:n) {
      T[i] ~ dcat(P[])                        # Outlier indicator variable.
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)   # True fish size.
      mu[i] <- log.alpha + beta * log(xt[i]); # Mean weight prediction.
      sigma.obs[i] <- sigma[T[i]]   
      
      # Calculate probability mass for an weight interval:
      pp[i] <- phi((log(y[i]+0.5) - mu[i]) / sigma.obs[i]) - phi((log(y[i]-0.5) - mu[i]) / sigma.obs[i])

      # Collapsed multinomial for a single observation:
      z[i] ~ dbern(pp[i])
   }
})

# Allometric model with:
#    - errors in observed sizes.
#    - outlier error term.
#    - weight precision taken into account.
#    - maturity effect modifier.
code <- nimbleCode({
   # Allometric parameter priors:
   log.alpha[1] ~ dnorm(0, sd = 100);  # Intercept parameter.
   delta.log.alpha ~ dunif(0, 10);     
   log.alpha[2] <- log.alpha[1] + delta.log.alpha;
   beta ~ dnorm(3.0, 1);               # Slope parameter.
   
   # Error process priors:
   sigma[1] ~ dunif(0, 100);           # Observation error.
   delta.sigma ~ dunif(0, 100);
   sigma[2] <- sigma[1] + delta.sigma; # Outlier error.
   
   # Prior over outlier probabilities:
   P.out[1:2] ~ ddirich(alpha[])
   #P.mat[1:2] ~ ddirich(alpha[])
   
   # Model definition:
   for (i in 1:n) {
      # Random indicator variables:    
      T[i] ~ dcat(P.out[1:2])                       # Outlier indicator variable.
      P.mat[i,1] <- 1 / (1 + exp(-(x[i]-xp.mat) / sigma.mat))
      P.mat[i,2] <- 1 - P.mat[i,1]                  # Maturity probability.
      M[i] ~ dcat(P.mat[i,1:2])                     # Maturity indicator variable.
      
      # Define log-linear mean and variance:
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)         # True fish size.
      #mu[i] <- log.alpha[M[i]] + beta * log(xt[i]); # Mean weight prediction.
      mu[i] <- log.alpha[1] + P.mat[i,1] * delta.log.alpha + beta * log(xt[i]); # Mean weight prediction.
      sigma.obs[i] <- sigma[T[i]]                   # Error term for weight observation.
      
      # Calculate probability mass for weight observation:
      pp[i] <- phi((log(y[i]+0.5) - mu[i]) / sigma.obs[i]) - phi((log(y[i]-0.5) - mu[i]) / sigma.obs[i])
      
      # Collapsed multinomial for a single observation:
      z[i] ~ dbern(pp[i])
   }
})

# Read September survey biological data:
bio <- read.gulf.bio(year = c(1971:2024), species = 43, password = password)
bio <- bio[which(bio$length < 100), ]
bio <- bio[which(bio$weight > 0), ]
bio <- bio[which(bio$weight < 10000), ]



# Create some constants, data, and initial values to pass to the model builder
constants <- list(n = nrow(bio), alpha = c(1,1))

data <- list(x = bio$length, 
             y = bio$weight,
             z = rep(1, nrow(bio)))

inits <- list(log.alpha = -5.3, delta.log.alpha = 0.05,
              beta = 3.1, sigma = c(0.1, NA), delta.sigma = 0.35, P.out = c(0.95, 0.05),
              xp.mat = 30, sigma.mat = 1)

mcmc <- nimbleMCMC(code, data = data, inits = inits, constants = constants,
                   monitors = c("log.alpha", "delta.log.alpha", "beta", "sigma", "P.out", "xp.mat", "sigma.mat"), 
                   thin = 10, niter = 100500, nburnin = 500, nchains = 1)

mcmc <- mcmc[-(1:500), ]

plot(mcmc[, "beta"], type = "l")
plot(mcmc[, "log.alpha[1]"], type = "l")
lines(1:nrow(mcmc), mcmc[, "log.alpha[2]"], col = "red")
plot(mcmc[, "sigma[1]"], type = "l")
plot(mcmc[, "sigma[2]"], type = "l")
plot(mcmc[, "xp.mat"], type = "l")
plot(mcmc[, "sigma.mat"], type = "l")
plot(mcmc[, "delta.log.alpha"], type = "l")
plot(mcmc[, "P.out[1]"], type = "l")

xx <- seq(1, 50, by = 1)
mu <- matrix(NA, nrow = nrow(mcmc), ncol = length(xx))
colnames(mu) <- xx
for (i in 1:length(xx)){
   mu[i, ] <- mcmc[i,"log.alpha"] + mcmc[i,"beta"] * log(xx)
}

plot(log(jitter(bio$length, amount = 0.5)), jitter(log(bio$weight), amount = 0.5), cex = 0.5)
points(log(bio$length), log(bio$weight), pch = 21, bg = fade("red"), col = fade("red"), cex = 1.0)
polygon(log(c(xx, rev(xx))), 
        c(apply(mu, 2, quantile, p = 0.025, na.rm = TRUE), rev(apply(mu, 2, quantile, p = 0.975, na.rm = TRUE))),
        col = fade("red"), border = fade("red"))

abline(mean(mcmc[, "log.alpha"]), mean(mcmc[, "beta"]), col = "red", lwd = 2)
abline(coef(model)[1], coef(model)[2], col = "green2", lwd = 2)


r <- log(data$y) - (mean(mcmc[, "log.alpha"]) + mean(mcmc[, "beta"]) * log(data$x))
boxplot(r[bio$sex == 1] ~ data$x[bio$sex == 1], ylim = c(-0.5, 0.5))
hline(0, col = "red")

bio$is.large.female <- (bio$length > 30 & bio$sex == 2) + 1 - 1
model <- lm(log(bio$weight) ~ log(bio$length))

model <- lm(log(bio$weight) ~ log(bio$length) + bio$is.large.female + as.factor(bio$sex))
r <- residuals(model)
boxplot(r[bio$sex == 1] ~ bio$length[bio$sex == 1], ylim = c(-0.5, 0.5))
hline(0, col = "red")
boxplot(r[bio$sex == 2] ~ bio$length[bio$sex == 2], ylim = c(-0.5, 0.5))
hline(0, col = "red")
table(bio$length, bio$sex)

ix <- bio$sex == 1
plot(log(jitter(bio$length[ix], amount = 0.5)), jitter(log(bio$weight[ix]), amount = 0.5), xlim = c(2, 5), cex = 0.5)
ix <- bio$sex == 2
points(log(jitter(bio$length[ix], amount = 0.5)), jitter(log(bio$weight[ix]), amount = 0.5), pch = 21, 
       bg = fade("red"), col = fade("red"), cex = 0.5)


code <- nimbleCode({
   mu ~ dnorm(0, sd = 1000)
   sigma ~ dunif(0, 1000)
   for(i in 1:10) {
      x[i] ~ dnorm(mu, sd = sigma)
   }
})
data <- list(x = c(2, 5, 3, 4, 1, 0, 1, 3, 5, 3))
inits <- function() list(mu = rnorm(1,0,1), sigma = runif(1,0,10))
mcmc.output <- nimbleMCMC(code, data = data, inits = inits,
                          monitors = c("mu", "sigma"), thin = 10,
                          niter = 20000, nburnin = 1000, nchains = 3,
                          summary = TRUE, WAIC = TRUE)


