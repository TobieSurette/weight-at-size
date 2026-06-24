library(gulf.data)
library(gulf.graphics)

# Analysis controls:
years <- 1970:2025
sex <- 2
degree <- 3
remove.outliers <- TRUE

# Read and format data:
x <- read.gulf.bio(year = years, password = password, species = 40)
x <- x[which((x$weight > 0) & (x$weight < 4000)), ]
x <- x[which((x$length > 0) & (x$length < 70)), ]
x$log.length <- log(x$length)
x$log.weight <- log(x$weight)
x$year <- as.numeric(substr(x$date, 1, 4))
x <- x[which(x$sex == sex), ]

# Identify outliers:
loglike <- function(theta, x, y){
   p <- 1 / (1 + exp(-theta[["logit.p"]]))
   mu <- theta[["log.alpha"]] + theta[["beta"]] * x
   sigma <- exp(theta[["log.sigma"]])
   sigma.outlier <- sigma + exp(theta[["log.sigma.delta"]])
   ll <- log(p * dnorm(y, mu, sigma) + (1-p) * dnorm(y, mu, sigma.outlier))
   return(-sum(ll))
}
# Initialize parameters:
model <- lm(x$log.weight ~ x$log.length)
theta <- coef(model)
names(theta) <- c("log.alpha", "beta")
theta <- c(theta, log.sigma = log(summary(model)$sigma), log.sigma.delta = log(summary(model)$sigma), logit.p = 3)
loglike(theta, x$log.length, x$log.weight)
theta <- optim(theta, loglike, x = x$log.length, y = x$log.weight, control = list(trace = 3))$par
# I
p <- 1 / (1 + exp(-theta[["logit.p"]]))
mu <- theta[["log.alpha"]] + theta[["beta"]] * x$log.length
sigma <- exp(theta[["log.sigma"]])
sigma.outlier <- sigma + exp(theta[["log.sigma.delta"]])
ll <- ((1-p) * dnorm(x$log.weight, mu, sigma.outlier)) / ((p * dnorm(x$log.weight, mu, sigma) + (1-p) * dnorm(x$log.weight, mu, sigma.outlier)))

# Outlier plot:
clg()
ix <- ll < 0.95  # Probability of being an outlier threshold.
xx <- log(jitter(x$length, amount = 0.5))
yy <- log(jitter(x$weight, amount = 0.5))
plot(c(1.0, 4.5), c(-1, 9), type = "n", 
     xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
points(xx[ix], yy[ix], cex = 0.1, col = fade("grey40", 0.25), lwd = 0.2)
points(xx[!ix], yy[!ix], cex = ll, col = fade("red", 0.25), lwd = 2)
abline(theta[["log.alpha"]], theta[["beta"]], lwd = 3, col = fade("blue"))
mtext("Length (cm)", 1, 2.5, font = 2, cex = 1.25)
mtext("Weight (g)", 2, 2.5, font = 2, cex = 1.25)
at <- 2^(0:12)
axis(1, at = log(at), labels = at)
axis(2, at = log(at), labels = at)
hline(log(at), lwd = 0.5, lty = "dashed", col = "grey70")
vline(log(at), lwd = 0.5, lty = "dashed", col = "grey70")
box(col = "grey50")

# Length x year deviations from robust allometric model:
r <- x$log.weight - theta[["log.alpha"]] - theta[["beta"]] * x$log.length
tmp <- aggregate(r, by = x[c("length", "year")], mean)
tab <- matrix(NA, nrow = 66, ncol = length(years))
dimnames(tab) <- list(length = 0:65, year = years)
tmp <- tmp[tmp$length <= 65, ]
for (i in 1:nrow(tmp)) tab[as.character(tmp$length[i]), as.character(tmp$year[i])] <- tmp$x[i]
breaks <- seq(-0.3, 0.3, by = 0.01)
tab[tab < min(breaks)] <- min(breaks)
tab[tab > max(breaks)] <- max(breaks)
cols <- colorRampPalette(c("red", "white", "blue"))(length(breaks)-1)
image(years, 0:65, t(tab), breaks = breaks, col = cols, xlab = "", ylab = "")
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Length (cm)", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Fit annual models:
x$year <- as.numeric(substr(x$date, 1, 4))
years <- sort(unique(x$year))
tab <- matrix(NA, nrow = length(years), ncol = 61)
dimnames(tab) <- list(year = years, length = 0:60)
beta <- NULL
x$residuals <- NA
for (i in 1:length(years)){
   ix <- which((x$sex == sex) & (x$year == years[i]))
   model <- lm(x$log.weight[ix] ~ poly(x$log.length[ix], degree = degree, raw = TRUE))
   beta <- rbind(beta, coef(model))
   x$residuals[ix] <- residuals(model)
}

# Overlay curves:
ix <- which(x$sex == sex)
beta.ref <- as.numeric(coef(lm(x$log.weight[ix] ~ x$log.length[ix])))
plot(log(jitter(x$length[ix], amount = 0.5)),
     log(jitter(x$weight[ix], amount = 0.5)) - (beta.ref[1] + beta.ref[2]*log(jitter(x$length[ix], amount = 0.5))), 
     xlab = "", ylab = "", xlim = c(1.5, 4.5), xaxs = "i", ylim = c(-1, 1), yaxs = "i",     col = fade("grey20", 0.25), cex = 0.2)
grid()
hline(0, col = fade("red"), lty = "dashed", lwd = 2)
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
t <- seq(1, 5, by = 0.01)
for (i in 1:nrow(beta)){
   b <- as.numeric(beta[i, ])
   y <- b[1] + b[2] * t + b[3] * t^2 + b[4] * t^3
   lines(t, y - beta.ref[1] - beta.ref[2] * t, col = fade("red"))
}
box(col = "grey50")

# Year residual plot:
boxplot(x$residuals ~ x$year, cex = 0.2, ylim = c(-0.5, 0.5), 
        xlab = "", ylab = "", border = "grey30")
hline(0, col = fade("red"), lty = "dashed", lwd = 2)
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Length residual plot:
boxplot(x$residuals ~ x$length, cex = 0.2, ylim = c(-0.5, 0.5), 
        xlab = "", ylab = "", border = "grey30")
hline(0, col = fade("red"), lty = "dashed", lwd = 2)
mtext("Length (cm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Length * year residuals:
tmp <- aggregate(x$residuals, by = x[c("length", "year")], mean)
tmp <- tmp[(tmp$length > 0 & tmp$length < 65), ]
tab <- matrix(NA, nrow = 66, ncol = length(years))
dimnames(tab) <- list(length = 0:65, year = years)
for (i in 1:nrow(tmp)) tab[as.character(tmp$length[i]), as.character(tmp$year[i])] <- tmp$x[i]
breaks <- seq(-0.3, 0.3, by = 0.01)
tab[tab < min(breaks)] <- min(breaks)
tab[tab > max(breaks)] <- max(breaks)
cols <- colorRampPalette(c("red", "white", "blue"))(length(breaks)-1)
image(as.numeric(colnames(tab)), as.numeric(rownames(tab)), t(tab), breaks = breaks, col = cols, xlab = "", ylab = "")
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Length (cm)", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Fit mixed effect model:
library(glmmTMB)
x$yearf <- as.factor(x$year)
m <- list()
ix <- which(ll < 0.95)  # Probability of being an outlier threshold.
m[[1]] <- glmmTMB(log.weight ~ log.length, data = x[ix,])
m[[2]] <- glmmTMB(log.weight ~ poly(log.length, degree = 2, raw = TRUE), data = x[ix,])
m[[3]] <- glmmTMB(log.weight ~ poly(log.length, degree = 3, raw = TRUE), data = x[ix,])
m[[4]] <- glmmTMB(log.weight ~ log.length + (1|yearf), data = x[ix,])
m[[5]] <- glmmTMB(log.weight ~ (log.length|yearf), data = x[ix,])
m[[6]] <- glmmTMB(log.weight ~ poly(log.length, degree = 2, raw = TRUE) + (poly(log.length, degree = 2, raw = TRUE)|yearf), data = x[ix,])
m[[7]] <- glmmTMB(log.weight ~ poly(log.length, degree = 3, raw = TRUE) + (poly(log.length, degree = 3, raw = TRUE)|yearf), data = x[ix,])

# Coefficients from lowest to highest degree: c(c0, c1, c2, ...)
horner_eval <- function(beta, x) {
   v <- 0
   for (i in rev(beta)) v <- v * x + i
   return(v)
}

model <- m[[7]]
rbeta <- ranef(model)[[1]][[1]]
fbeta <- fixef(model)[[1]]
beta <- coef(model)[[1]][[1]]
xx <- log(jitter(x$length, amount = 0.5))
yy <- log(jitter(x$weight, amount = 0.5))
ix <- which(ll < 0.95)
plot(xx[ix],
     yy[ix] - theta[["log.alpha"]] - theta[["beta"]]*xx[ix], 
     xlab = "", ylab = "", xlim = c(1.5, 4.5), xaxs = "i", ylim = c(-1.5, 1.5), yaxs = "i",     
     col = fade("grey20", 0.25), cex = 0.2)
grid()
tx <- seq(1.5, 4.5, len = 100)
for (i in 1:nrow(beta)){
   ty <- horner_eval(beta[i, ], tx) - theta[["log.alpha"]] - theta[["beta"]]*tx
   lines(tx, ty, col = fade("red"), lwd = 1)
}
lines(tx, horner_eval(fbeta, tx) - theta[["log.alpha"]] - theta[["beta"]]*tx, col = fade("blue"), lwd = 2)
mtext("Length (cm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Year residual plot:
boxplot(residuals(model) ~ year, data = x[ix, ], cex = 0.2, ylim = c(-0.5, 0.5), 
        xlab = "", ylab = "", border = "grey30")
hline(0, col = fade("red"), lty = "dashed", lwd = 2)
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Length residual plot:
boxplot(residuals(model) ~ length, data = x[ix, ], cex = 0.2, ylim = c(-0.5, 0.5), 
        xlab = "", ylab = "", border = "grey30")
hline(0, col = fade("red"), lty = "dashed", lwd = 2)
mtext("Length (cm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Residuals", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Length * year residuals:
tmp <- aggregate(residuals(model), by = x[ix, c("length", "year")], mean)
tmp <- tmp[(tmp$length > 0 & tmp$length < 65), ]
tab <- matrix(NA, nrow = 66, ncol = length(years))
dimnames(tab) <- list(length = 0:65, year = years)
for (i in 1:nrow(tmp)) tab[as.character(tmp$length[i]), as.character(tmp$year[i])] <- tmp$x[i]
breaks <- seq(-0.3, 0.3, by = 0.01)
tab[tab < min(breaks)] <- min(breaks)
tab[tab > max(breaks)] <- max(breaks)
cols <- colorRampPalette(c("red", "white", "blue"))(length(breaks)-1)
image(as.numeric(colnames(tab)), as.numeric(rownames(tab)), t(tab), breaks = breaks, col = cols, xlab = "", ylab = "")
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Length (cm)", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

