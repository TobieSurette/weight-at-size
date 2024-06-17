library(gulf.data)
library(gulf.graphics)

z <- read.scsbio(2013:2014)
z <- z[which(!is.na(z$weight) & (z$carapace.width >= 95)), ]
z$maturity <- is.mature(z)
ix <- is.na(z$maturity)
z$maturity[ix] <- is.mature(z, probability = TRUE)[ix] > 0.5
z$missing.legs <- gsub("7", "2", z$missing.legs)

# Parse missing leg pattern:
P <- matrix(NA, nrow = nrow(z), ncol = 10)
tmp <- strsplit(z$missing.legs, "")
for (i in 1:10) P[, i] <- as.numeric(unlist(lapply(tmp, function(x) x[i])))
P[is.na(P)] <- 0   

M <- matrix(NA, nrow = nrow(z), ncol = 5)  # Number of missing legs.
for (i in 1:5) M[,i] <- (P[,i] == 1) + (P[,i+5] == 1)
R <- matrix(NA, nrow = nrow(z), ncol = 5)  # Number of regenerating legs.
for (i in 1:5) R[,i] <- (P[,i] == 2) + (P[,i+5] == 2)
C <- (2-M-R)  # Number of complete legs,
        
# Winbugs export:
ix <- sample(1:nrow(z), 500)
ix <- ix[order(z$carapace.width[ix])]
clc()
cat(paste0("n = ", length(ix), ", \n"))
cat("x = c(", paste0(round(z$carapace.width[ix], 1), collapse = ","), "),")
cat("w = c(", paste0(round(z$weight[ix]), collapse = ","), "),")
cat("mat = c(", paste0(round(z$maturity[ix]), collapse = ","), "),")

cat("shell = c(", paste0(round(z$shell.condition[ix]), collapse = ","), "),")

cat("C = structure(.Data = c(\n")
for (i in 1:length(ix)){
   cat("    ", paste0(paste0(C[ix[i], ], collapse = ","), ",\n"))
}
cat("), \n    .Dim = ", paste0("c(", length(ix), ",", 5, "))"))
cat("R = structure(.Data = c(\n")
for (i in 1:length(ix)){
        cat("    ", paste0(paste0(R[ix[i], ], collapse = ","), ",\n"))
}
cat("), \n    .Dim = ", paste0("c(", length(ix), ",", 5, "))"))


plot(z$carapace.width, z$weight, pch = 19, cex = 0.3, 
     xlim = c(95, 135), xaxs = "i", ylim = c(200, 900), yaxs = "i", xlab = "", ylab = "")
grid()
ix <- z$maturity
points(z$carapace.width[ix], z$weight[ix], pch = 19, cex = 0.2, col = "red")
points(z$carapace.width[!ix], z$weight[!ix], pch = 19, cex = 0.3, col = "green3")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Weight (g)", 2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = c("Mature", "Immature"),
       pch = 19, col = c("red", "green3"), border = "grey50")
box(col = "grey50")   

# By maturity and shell condition:
plot(z$carapace.width, z$weight, pch = 19, cex = 0.3, xlim = c(95, 135), xaxs = "i", ylim = c(200, 900), yaxs = "i", xlab = "", ylab = "")
grid()
ix <- z$maturity
points(z$carapace.width[ix], z$weight[ix], pch = 19, cex = 0.6, col = "red")
points(z$carapace.width[!ix], z$weight[!ix], pch = 19, cex = 0.6, col = "green3")
ix <- z$maturity & z$shell.condition > 2
points(z$carapace.width[ix], z$weight[ix], pch = 19, cex = 0.6, col = "purple")
ix <- !z$maturity & z$shell.condition > 2
points(z$carapace.width[ix], z$weight[ix], pch = 19, cex = 0.6, col = "blue")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Weight (g)", 2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = c("Mature New", "Mature Old", "Immature New", "Immature Old"),
       pch = 19, col = c("red", "purple", "green3", "blue"), border = "grey50")
box(col = "grey50") 

# Intact crabs by maturity and shell condition:
ix <- which((z$missing.legs == "**********") & (z$comments == "*"))
plot(z$carapace.width[ix], z$weight[ix], pch = 19, cex = 0.3, xlim = c(95, 135), xaxs = "i", ylim = c(200, 900), yaxs = "i", xlab = "", ylab = "")
grid()
iy <- which((z$missing.legs == "**********") & (z$comments == "*") & !z$maturity)
points(z$carapace.width[iy], z$weight[iy], pch = 19, cex = 0.6, col = "green3")
iy <- which((z$missing.legs == "**********") & z$maturity & (z$shell.condition >= 3))
points(z$carapace.width[iy], z$weight[iy], pch = 19, cex = .6, col = "red2")
iy <- which((z$missing.legs == "**********") & z$maturity & (z$shell.condition < 3))
points(z$carapace.width[iy], z$weight[iy], pch = 19, cex = 0.6, col = "blue")
iy <- which((z$missing.legs != "**********") & z$maturity & (z$shell.condition >= 3))
points(z$carapace.width[iy], z$weight[iy], pch = 19, cex = 0.8, col = "purple")
mtext("Carapace width (mm)", 1, 2.5, cex = 1.25, font = 2)
mtext("Weight (g)", 2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = c("Mature New", "Mature Old", "Immature New", "Immature Old"),
       pch = 19, col = c("red", "purple", "green3", "blue"), border = "grey50")
box(col = "grey50") 

z <- z[iy, ]


# BUGS output:
iy <- which((z$missing.legs == "**********") & (z$comments == "*") & (z$shell.condition >= 3))
iy <- which((z$missing.legs == "**********") & (z$comments == "*"))
iy <- which((z$missing.legs %in% c("**********", "1*********", "*****1****")) & (z$comments == "*"))
iy <- which((apply(R, 1, sum) == 0) & (z$comments == "*"))
clc()
file <- "data/SCS weight data.txt"
cat("list(\n", file = file)
cat(paste0("n = ", length(iy), ", \n"), file = file, append = TRUE)
cat(paste0("theta.outlier = c(1,1), \n"), file = file, append = TRUE)
cat("log.x = c(", paste0(round(log(z$carapace.width[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("log.w = c(", paste0(round(log(z$weight[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("maturity = c(", paste0(round(z$maturity[iy],3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("hardness = c(", paste0(round((z$shell.condition[iy] >= 3)), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("missing = structure(.Data = c(\n", file = file, append = TRUE)
cat("    ", paste0(paste0(as.numeric(t(M[iy, ])), collapse = ",")), file = file, append = TRUE)
cat("), \n    .Dim = ", paste0("c(", length(iy), ",", 5, "))\n"), file = file, append = TRUE)
cat(")", file = file, append = TRUE)

zz <- z[iy,]
# Residual analysis:
res <- read.csv("results/SCS residuals.csv")


plot(res$mean)
z$julian <- julian(date(z))
boxplot(res$mean ~ julian(date(zz)), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean ~ week(date(zz)), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean ~ round(zz$carapace.width), ylim = c(-1, 1))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean[zz$maturity] ~ julian(date(zz[zz$maturity, ])), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean[!zz$maturity] ~ julian(date(zz[!zz$maturity, ])), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)

zz$hardness <- zz$shell.condition >= 3

boxplot(res$mean[zz$hardness] ~ julian(date(zz[zz$hardness, ])), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean[!zz$hardness] ~ julian(date(zz[!zz$hardness, ])), ylim = c(-2, 2))
hline(0, col = "red2", lwd = 2)


tmp <- aggregate(list(res = res$mean[!zz$hardness]), list(day = julian(date(zz[!zz$hardness, ]))), mean)

fun <- function(x, a = 1, xp = 0) return(a*((1 / (x-xp)^b) - 1))
t <- seq(0, 1, len = 1000)
plot(t, fun(t, a = 1, b = 1), type = "l", ylim = c(0, 20))

loglike <- function(theta, x, y){
   mu <- theta["a"] * exp(-theta["b"] * (x-theta["xp"]))
   return(sum((y - mu)^2))
} 

theta <- c(xp = 200, a = -0.1, b = 0.1)
loglike(theta, tmp$day, tmp$res)
theta <- optim(theta, loglike, x = tmp$day, y = tmp$res, control = list(trace = 3))$par

theta <- c(xp = 230, a = -0.1, b = 0.07)
plot(tmp$day, tmp$res)
t <- seq(0, 360, len = 1000)
lines(t, theta["a"] * exp(-theta["b"] * (t-theta["xp"])))


boxplot(res$mean[zz$hardness] ~ round(zz$carapace.width[zz$hardness]), ylim = c(-1, 1))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean[!zz$hardness] ~ round(zz$carapace.width[!zz$hardness]), ylim = c(-1, 1))
hline(0, col = "red2", lwd = 2)

boxplot(res$mean ~ year(date(zz)), ylim = c(-1, 1))
hline(0, col = "red2", lwd = 2)

# 
tmp <- aggregate(list(res = res$mean[!zz$hardness]), 
                 by = list(cw = round(zz$carapace.width[!zz$hardness]), julian = julian(date(zz))[!zz$hardness]),
                 mean)
ix <- which(tmp$res > 0)
plot(tmp$julian[ix], tmp$cw[ix], pch = 21, bg = "grey60", cex = 1.5 * sqrt(tmp$res[ix]))
points(tmp$julian[-ix], tmp$cw[-ix], pch = 21, bg = "red2", cex = 1.5 * sqrt(-tmp$res[-ix]))


# Spatial residuals:
library(gulf.spatial)
tmp <- aggregate(list(res = res$mean), by = zz[c("date", "tow.id")], mean)
s <- read.scsset(year = 2013:2014, valid = 1)

ix <- match(tmp[c("date", "tow.id")], s[c("date", "tow.id")])
s$res <- NA
s$res[ix] <- tmp$res

year <- 2013
map.new()
ix <- which(year(s) %in% year & (s$res > 0))
points(lon(s)[ix], lat(s)[ix], cex = 1.5 * sqrt(s$res[ix]), pch = 21, bg = "grey60")
ix <- which(year(s) %in% year & (s$res < 0))
points(lon(s)[ix], lat(s)[ix], cex = 1.5 * sqrt(-s$res[ix]), pch = 21, bg = "red2")
map("coast")
box(col = "grey50")





