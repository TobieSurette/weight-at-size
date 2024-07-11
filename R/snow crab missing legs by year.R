library(gulf.data)

# Load data:
b <- read.scsbio(1997:2023, valid = 1, survey = "regular", sex = 1)
b <- b[which(is.mature(b) & (b$carapace.width >= 95)), ]

# Parse missing and regenerated leg pattern:
M <- matrix(NA, nrow = nrow(b), ncol = 10)
colnames(M) <- c(paste0("L", 1:5), paste0("R", 1:5))
R <- M
for (i in 1:10){
   M[,i] <- substr(b$missing.legs, i, i) == "1"
   R[,i] <- substr(b$missing.legs, i, i) == "2"
} 

# Calculate summary statistics:
res <- aggregate(M[,1:5] + M[,6:10], by = list(year = year(b)), mean)


png(file = "figures/snow crab missing leg by year.png",
    units = "in", res = 500, height = 6.0, width = 7.5)

cols <- c("tomato", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4")
lty <- c("solid", "dashed", "dotted", "dotdash", "twodash")
plot(range(res$year), c(0, 0.20), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
for (i in c(2:5,1)){
   lines(res$year, res[, i+1], col = cols[i], lty = lty[i], lwd = 2)
}
mtext("Year",  1, 2.5, cex = 1.25, font = 2)
mtext("Missing rate (# / crab)",  2, 2.5, cex = 1.25, font = 2)
legend("topleft", 
       legend = c("1st walking leg", "2nd walking leg", "3rd walking leg", "4th walking leg", "Cheliped"),
       col = cols[c(2:5, 1)], lty = lty[c(2:5, 1)],
       lwd = 2, cex = 0.95,
       text.font = 1)
box(col = "grey50")
dev.off()


