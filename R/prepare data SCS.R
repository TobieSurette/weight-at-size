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

# BUGS output:
zz <- z[z$maturity, ]
clc()
paste0(c("x = c(", round(z$carapace.width,1), ")"), collapse = ",")
paste0(c("w = c(", round(z$weight), ")"), collapse = ",")

