library(gulf.data)
library(gulf.graphics)

# Load RV data:
bio <- read.gulf.bio(year = 2012:2017, species = 2526, password = password)
bio <- bio[which((bio$sex == 1) & (!is.na(bio$length)) & (bio$weight > 0) & !is.na(bio$weight)), ]

# Remove damaged crab:
ix <- grep("damag", tolower(bio$comment))
ix <- c(ix, grep("crush", tolower(bio$comment)))
ix <- c(ix, grep("smash", tolower(bio$comment)))
ix <- c(ix, grep("miss", tolower(bio$comment)))
ix <- c(ix, grep("miss", tolower(bio$comment)))
ix <- c(ix, grep("mssi", tolower(bio$comment)))
ix <- c(ix, grep("broke", tolower(bio$comment))) 
ix <- c(ix, grep("manque", tolower(bio$comment))) 
bio <- bio[-ix, ]

# Fill-in missing leg data from comments:
ix <- which(is.na(bio$missing.legs))
bio$missing.legs[ix] <- missing.legs(bio$comment[ix])
bio <- bio[which(nchar(bio$missing.legs) == 10), ]

# Check that missing legs were consistently observed:
#table(year(bio$date), bio$missing.legs == "**********") 
#table(year(bio$date), julian(date(bio$date)) , bio$missing.legs == "**********") 

# Remove period in 2014 where no missing leg info was recorded:
ix <- which((year(bio$date) == 2014) & (julian(date(bio$date)) >= 263))
bio <- bio[-ix, ]

# Check chela data:
table(year(bio$date), julian(date(bio$date)), is.na(bio$chela))

plot(jitter(bio$length, amount = 0.5), jitter(bio$chela, amount = 0.5), xlim = c(0, 140), xaxs = "i", ylim = c(0, 45), yaxs = "i", cex = 0.2)
plot(jitter(bio$length, amount = 0.5), jitter(bio$chela, amount = 0.5), xlim = c(40, 140), xaxs = "i", ylim = c(0, 40), yaxs = "i", cex = 0.2)

gbarplot(table(bio$length[is.na(bio$chela)]))

# Check weight rounding pattern:
gbarplot(table(bio$weight %% 10))

# Parse missing leg pattern:
M <- matrix(NA, nrow = nrow(bio), ncol = 10)
tmp <- strsplit(bio$missing.legs, "")
for (i in 1:10) M[, i] <- as.numeric(unlist(lapply(tmp, function(x) x[i])))
M[is.na(M)] <- 0   

plot(log(jitter(bio$length, amount = 0.5)), log(jitter(bio$weight, amount = 0.5)), cex = 0.3, xlim = c(2.5, 5))

ix <- apply(M, 1, sum) == 1
points(log(jitter(bio$length[ix], amount = 0.5)), log(jitter(bio$weight[ix], amount = 0.5)), col = "red", cex = 0.5)
ix <- apply(M, 1, sum) == 2
points(log(jitter(bio$length[ix], amount = 0.5)), log(jitter(bio$weight[ix], amount = 0.5)), col = "green2", cex = 0.5)

ix <- apply(M, 1, sum) == 3
points(log(jitter(bio$length[ix], amount = 0.5)), log(jitter(bio$weight[ix], amount = 0.5)), col = "blue", cex = 0.5)
ix <- apply(M, 1, sum) == 4
points(log(jitter(bio$length[ix], amount = 0.5)), log(jitter(bio$weight[ix], amount = 0.5)), col = "purple", cex = 0.5)


plot(jitter(bio$length, amount = 0.5), jitter(bio$weight, amount = 0.5), cex = 0.3, xlim = c(0, 140), ylim = c(0, 1200))
ix <- apply(M == 1, 1, sum) == 1
points(jitter(bio$length[ix], amount = 0.5), jitter(bio$weight[ix], amount = 0.5), col = "red", cex = 0.5)
ix <- apply(M == 1, 1, sum) == 2
points(jitter(bio$length[ix], amount = 0.5), jitter(bio$weight[ix], amount = 0.5), col = "green2", cex = 0.5)
ix <- apply(M == 1, 1, sum) == 3
points(jitter(bio$length[ix], amount = 0.5), jitter(bio$weight[ix], amount = 0.5), col = "blue", cex = 0.5)
ix <- apply(M == 1, 1, sum) == 4
points(jitter(bio$length[ix], amount = 0.5), jitter(bio$weight[ix], amount = 0.5), col = "purple", cex = 0.5)

ix <- apply(M == 2, 1, sum) == 2
points(jitter(bio$length[ix], amount = 0.5), jitter(bio$weight[ix], amount = 0.5), col = "red", cex = 0.5)

ix <- apply(M == 0, 1, sum) == 10
xx <- log(bio$length[ix])
yy <- log(bio$weight[ix])

loglike <- function(theta, x, y){
   mu <- theta["intercept"] + theta["slope"] * x
   sigma <- exp(theta["log.sigma0"])
   sigma <- c(sigma, sigma + exp(theta["log.sigma1"]))
   p <- 1 / (1 + exp(-theta["logit.p"]))
   ll <- log((1-p) * dnorm(yy, mu, sigma[1]) + p * dnorm(yy, mu, sigma[2]))
   
   return(-sum(ll))
}

xx <- log(bio$length)
yy <- log(bio$weight)
theta <- c(intercept = -7.8284165, slope = 2.9881278, log.sigma0 = -2.3479475, log.sigma1 =  0.2080461, logit.p = -2.094737)
theta <- optim(theta, loglike, x = xx, y = yy, control = list(trace = 3, maxit = 2000))$par 

# Residuals:
mu <- theta["intercept"] + theta["slope"] * xx
r <- yy - mu  
plot(log(jitter(bio$length, amount = 0.5)), r, cex = 0.4, xlim = c(3.5, 5), ylim = c(-1, 1))
hline(0, col = "red", lwd = 2)
ix <- apply(M == 1, 1, sum) == 1
points(log(jitter(bio$length[ix], amount = 0.5)), r[ix], col = "red3", cex = 0.3)
ix <- apply(M == 1, 1, sum) == 2
points(log(jitter(bio$length[ix], amount = 0.5)), r[ix], col = "green3", cex = 0.5)
ix <- apply(M == 1, 1, sum) == 3
points(log(jitter(bio$length[ix], amount = 0.5)), r[ix], col = "blue", cex = 0.7)


