library(gulf.data)
library(nimble)
library(gulf.graphics)

# Read September survey biological data:
bio <- read.gulf.bio(year = c(1971:2024), species = 43, password = password)
bio <- bio[which(bio$length < 100), ]
bio <- bio[which(bio$weight > 0), ]
bio <- bio[which(bio$weight < 10000), ]
bio$year <- year(bio$date)
bio$precision.weight <- 1
bio$precision.weight[bio$year %in% 1971:1994] <- 5
bio$precision.weight[bio$year %in% 1995:2015] <- 2
bio$precision.weight[bio$year > 2015] <- 1
bio$sex[!(bio$sex %in% 1:2)] <- NA

# Define weight precision (shift weight precision interval away from zero):
bio$precision.weight[bio$weight < 5 & bio$year %in% 1971:1994] <- 1
bio$weight.lower <- bio$weight - bio$precision.weight / 2
bio$weight.upper <- bio$weight + bio$precision.weight / 2
bio$weight.lower[bio$weight.lower <= 0] <- 0.1
bio$weight <- (bio$weight.upper + bio$weight.lower) / 2
bio$precision.weight <- (bio$weight.upper - bio$weight.lower) / 2

# Residual filter to remove obvious outliers:
model <- lm(log(bio$weight) ~ log(bio$length))
bio <- bio[abs(residuals(model, type = "pearson")) < 1, ]
#bio <- bio[sort(sample(1:nrow(bio), 5000)), ]

# Weight rounding pattern:
t <- table(year(bio$date), bio$weight %% 10)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
logit <- function(x) return(log(x / (1-x)))
plot(c(1970.5, 2024.5), c(-0.5, 9.5), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
image(1971:2024, 0:9, logit(t), add = TRUE)
mtext("Year", 1, 2.5, font = 2, cex = 1.25)
mtext("Last digit", 2, 2.5, font = 2, cex = 1.25)
box(col = "grey")

# Length rounding pattern:
t <- table(year(bio$date), bio$length %% 10)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
logit <- function(x) return(log(x / (1-x)))
plot(c(1970.5, 2024.5), c(-0.5, 9.5), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
image(1971:2024, 0:9, logit(t), add = TRUE)
mtext("Year", 1, 2.5, font = 2, cex = 1.25)
mtext("Last digit", 2, 2.5, font = 2, cex = 1.25)
box(col = "grey")

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
   P.out[1:2] ~ ddirich(alpha[])
   
   # Model definition:
   for (i in 1:n) {
      T[i] ~ dcat(P.out[])                    # Outlier indicator variable.
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)   # True fish size.
      mu[i] <- log.alpha + beta * log(xt[i]); # Mean weight prediction.
      sigma.obs[i] <- sigma[T[i]]   
      
      # Calculate probability mass for an weight interval:
      pp[i] <- phi((log(y[i]+(precision[i]/2)) - mu[i]) / sigma.obs[i]) - 
               phi((log(y[i]-(precision[i]/2)) - mu[i]) / sigma.obs[i])
      
      # Collapsed multinomial for a single observation:
      z[i] ~ dbern(pp[i])
   }
})

# Allometric model with:
#    - errors in observed sizes.
#    - outlier error term.
#    - weight precision taken into account.
#    - Year effects.
code <- nimbleCode({
   # Allometric parameter priors:
   log.alpha.mu ~ dnorm(0, sd = 100);   # Intercept parameter global mean.
   log.alpha.tau ~ dgamma(1,1);         # Intercept parameter global precision.
   for (j in 1:n.year){
      log.alpha.year[j] ~ dnorm(0, log.alpha.tau);      # Intercept parameter.
      log.alpha[j] <- log.alpha.mu + log.alpha.year[j]; # Intercept parameter.
   }
   
   # Allometric slope parameter priors.
   beta.mu ~ dnorm(3.0, 1);               
   beta.tau ~ dgamma(1,1);        
   for (j in 1:n.year){
      beta.year[j] ~ dnorm(0, beta.tau);  
      beta[j] <- beta.mu + beta.year[j];     
   }
   
   # Sex effect:
   tau.sex ~ dgamma(1,1)
   sex.effect[1]  ~ dnorm(0, tau.sex)
   sex.effect[2]  ~ dnorm(0, tau.sex)
   P.sex[1:2] ~ ddirich(alpha[])
   for (i in 1:n){
      sex[i] ~ dcat(P.sex[1:2])
   }
   
   # Error process priors:
   sigma[1] ~ dunif(0, 100);           # Observation error.
   delta.sigma ~ dunif(0, 100);
   sigma[2] <- sigma[1] + delta.sigma; # Outlier error.
   
   # Prior over outlier probabilities:
   P.out[1:2] ~ ddirich(alpha[])
   
   # Model definition:
   for (i in 1:n) {
      T[i] ~ dcat(P.out[])                    # Outlier indicator variable.
      xt[i] ~ dunif(x[i] - 0.5, x[i] + 0.5)   # True fish size.
      mu[i] <- log.alpha[year[i]] + sex.effect[sex[i]] + beta[year[i]] * log(xt[i]); # Mean weight prediction.
      sigma.obs[i] <- sigma[T[i]]   
      
      # Calculate probability mass for an weight interval:
      pp[i] <- phi((log(y[i]+(precision[i]/2)) - mu[i]) / sigma.obs[i]) - 
               phi((log(y[i]-(precision[i]/2)) - mu[i]) / sigma.obs[i])
      
      # Collapsed multinomial for a single observation:
      z[i] ~ dbern(pp[i])
   }
})

# Create some constants, data, and initial values to pass to the model builder
constants <- list(n = nrow(bio), alpha = c(1,1), n.year = max(bio$year) - min(bio$year) + 1)

# Define data structure:
data <- list(x = bio$length, 
             y = bio$weight,
             precision = bio$precision.weight,
             year = bio$year - min(bio$year) + 1,
             sex  = bio$sex,
             z = rep(1, nrow(bio)))

# Define initial values:
inits <- list(log.alpha.mu = as.numeric(coef(lm(log(bio$weight) ~ log(bio$length)))[1]), 
              beta.mu = as.numeric(coef(lm(log(bio$weight) ~ log(bio$length)))[2]), 
              log.alpha.tau = 1,
              beta.tau = 1,
              log.alpha.year = rep(0, constants$n.year),
              beta.year = rep(0, constants$n.year),
              sigma = c(0.1, NA), 
              delta.sigma = 0.35, 
              tau.sex = 1,
              sex.effect = c(0,0),
              P.out = c(0.95, 0.05))

sim <- nimbleMCMC(code, 
                  data = data, 
                  inits = inits, 
                  constants = constants,
                  monitors = c("log.alpha.mu", "log.alpha.tau", "log.alpha", 
                               "beta.mu", "beta.tau", "beta",
                               "sex.effect", "tau.sex",
                               "sigma", "P.out", "P.sex"), 
                  thin = 5, niter = 25500, nburnin = 500, nchains = 1)

plot(sim[, "log.alpha[1]"], type = "l")
boxplot()

plot(bio$length, bio$weight)
ix <- bio$year == 1983
points(bio$length[ix], bio$weight[ix], pch = 21, bg = "red")

t <- table(bio$length, bio$year, bio$sex)
p <- t[,,1] / apply(t, 1:2, sum)
image(as.numeric(rownames(p)), 1971:2024,  p, col = rainbow(100))

plot(c(10, 40), c(0, 1), type = "n", xaxs = "i", yaxs = "i")
for (j in 1:ncol(p)){
   lines(as.numeric(rownames(p)), p[, j], col = rainbow(ncol(p))[j], lwd = 2)
}
hline(0.5, lwd = 2, col = "red")
