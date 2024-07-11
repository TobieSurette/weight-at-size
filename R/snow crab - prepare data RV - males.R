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

# Define variables:
source("C:/Users/SuretteTJ/Desktop/github/gulf.data/R/maturity.R")
bio$carapace.width <- bio$length
bio$chela.height <- bio$chela
bio$maturity <- is.mature.scsbio(bio)
bio <- bio[which((bio$carapace.width >= 40) & (bio$carapace.width <= 150)), ]
bio$julian <- julian(date(bio))

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

# Remove data with invalid chela heights:
plot(log(jitter(bio$length, amount = 0.5)), log(jitter(bio$chela, amount = 0.5)), 
     xlim = c(3.68, 5), ylim = c(0, 4.5),xaxs = "i",  yaxs = "i", cex = 0.2,
     xlab = "", ylab = "")
abline(-2.8, 1.34, lty = "dashed", col = "red", lwd = 2)
abline(-3.8, 1.34, lty = "dashed", col = "red", lwd = 2)
mtext("Carapace width (mm)", 1, 2.5, font = 2, cex = 1.25)
mtext("Chela height (mm)", 2, 2.5, font = 2, cex = 1.25)
box(col = "grey50")
ix <- which((log(bio$chela.height) - (1.34 * log(bio$carapace.width) - 2.8)) > 0 )
iy <- which((log(bio$chela.height) - (1.34 * log(bio$carapace.width) - 3.8)) < 0 )
points(log(bio$carapace.width)[ix], log(bio$chela.height)[ix], pch = 21, bg = "red", cex = 1)
points(log(bio$carapace.width)[iy], log(bio$chela.height)[iy], pch = 21, bg = "red", cex = 1)
bio <- bio[-c(ix, iy), ]
bio <- bio[!is.na(bio$maturity), ]

# Remove data with problematic comments:
ix <- unique(c(grep("bar", tolower(bio$comment)), grep("alg", tolower(bio$comment)), grep("car", tolower(bio$comment))))
bio <- bio[-ix, ]

# Remove problematic weight data:
plot(log(jitter(bio$carapace.width, amount = 0.5)), 
     log(jitter(bio$weight, amount = 0.5)), pch = 21, bg = "grey", cex = 0.2)
abline(-7.4, 3, lty = "dashed", col = "red", lwd = 2)
abline(-9.1, 3, lty = "dashed", col = "red", lwd = 2)
ix <- which((log(bio$weight) - (3 * log(bio$carapace.width) - 7.4)) > 0)
iy <- which((log(bio$weight) - (3 * log(bio$carapace.width) - 9.1)) < 0)
bio <- bio[-c(ix, iy), ]
points(log(jitter(bio$carapace.width, amount = 0.5))[iy], 
       log(jitter(bio$weight, amount = 0.5))[iy], col = "red")


# Parse missing leg pattern:
M <- missing.legs(bio$missing.legs)
R <- M == 2
M <- M == 1

M <- M[,1:5] + M[,6:10]

# Export data:
clc()
iy <- which(apply(R, 1, sum) == 0)
file <- "data/RVS weight data males.txt"
cat("list(\n", file = file)
cat(paste0("n = ", length(iy), ", \n"), file = file, append = TRUE)
cat(paste0("theta.outlier = c(1,1), \n"), file = file, append = TRUE)
cat("log.x = c(", paste0(round(log(bio$carapace.width[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("log.w = c(", paste0(round(log(bio$weight[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("maturity = c(", paste0(round(bio$maturity[iy]), collapse = ","), "),\n\n", file = file, append = TRUE)
#cat("hardness = c(", paste0(round((bio$shell.condition[iy] >= 3)), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("missing = structure(.Data = c(\n", file = file, append = TRUE)
cat("    ", paste0(paste0(as.numeric(t(M[iy, ])), collapse = ",")), file = file, append = TRUE)
cat("), \n    .Dim = ", paste0("c(", length(iy), ",", 5, "))\n"), file = file, append = TRUE)
cat(")", file = file, append = TRUE)


# Check weight rounding pattern:
gbarplot(table(bio$weight %% 10))




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




   