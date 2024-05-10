library(gulf.data)
library(gulf.graphics)

# Load data:
years <- 2010:2023
species(c(622,623,624,625,626,631,632))
data <- read.gulf.bio(year = years, species = 622, password = password)
data <- data[which((data$weight >= 1) & (data$weight <= 100)), ]
data <- data[which(data$length > 0), ]

# Define weight data precision:
data$year <- year(data$date)
data$precision <- 1
data$precision[data$year < 2000] <- 5
data$precision[data$year %in% 2000:2011] <- 2

# Plot size-weight data:
plot(log(jitter(data$length, amount = 0.5)), log(jitter(data$weight, amount = 0.5)))
abline(log(0.01749603), 2.12330328, lwd = 2, col = "red")

# Residual plot:
r <- log(jitter(data$weight, amount = 0.5)) - 2.12330 * log(jitter(data$length, amount = 0.5)) - log(0.01749603)
plot(r)
hline(0, lwd = 2, col = "red")

# Daubed shanny:
tmp <- read.gulf.bio(year = years, species = 632, password = password)
points(log(jitter(tmp$length, amount = 0.5)), log(jitter(tmp$weight, amount = 0.5)), pch = 21, bg = "red2", cex = 0.6)

# Source allometric function:
source("R/weight.at.size.R")

# Test model fitting procedures:
fit.allometric(data$length, data$weight, robust = FALSE, plot = TRUE)
mtext("Standard allometric regression", 3, 0.5, font = 2, cex = 1.25)

fit.allometric(data$length, data$weight, robust = FALSE, precision = data$precision, plot = TRUE)
mtext("Allometric regression with precision", 3, 0.5, font = 2, cex = 1.25)

fit.allometric(data$length, data$weight, robust = TRUE, plot = TRUE)
mtext("Allometric regression with extra error", 3, 0.5, font = 2, cex = 1.25)

fit.allometric(data$length, data$weight, robust = TRUE, precision = data$precision, plot = TRUE)
mtext("Allometric regression with precision and extra error", 3, 0.5, font = 2, cex = 1.25)

# Evaluate precision of weight measurements through the years:
t <- table(year(data$date), data$weight %% 10)
t <- t / repvec(apply(t, 1, sum), ncol = ncol(t))
image(x = sort(unique(year(data$date))),
      y = 0:9,  
      z = t,
      xlab = "", ylab = "", #zlim = c(0, 1),
      col = c("white",rev(grey.colors(20))))
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Last digit of weight measurement", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Size distributions by year:
t <- table(year(data$date), data$length)
tmp <- matrix(NA, nrow = length(setdiff(as.character(years), rownames(t))), ncol = ncol(t))
rownames(tmp) <- setdiff(as.character(years), rownames(t)) 
colnames(tmp) <- colnames(t)
t <- rbind(t,tmp)
t <- t[order(as.numeric(rownames(t))), ]
t[t == 0] <- NA
image(x = as.numeric(rownames(t)), y = as.numeric(colnames(t)), z = t,
      xlab = "", ylab = "", zlim = c(0, max(t, na.rm = TRUE)),
      col = rev(grey.colors(20)))
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Length (cm)", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

# Weight distributions by year:
t <- table(year(data$date), data$weight)
tmp <- matrix(NA, nrow = length(setdiff(as.character(years), rownames(t))), ncol = ncol(t))
rownames(tmp) <- setdiff(as.character(years), rownames(t)) 
colnames(tmp) <- colnames(t)
t <- rbind(t,tmp)
t <- t[order(as.numeric(rownames(t))), ]
t[t == 0] <- NA
image(x = as.numeric(rownames(t)), y = as.numeric(colnames(t)), z = t,
      xlab = "", ylab = "", zlim = c(0, max(t, na.rm = TRUE)),
      col = rev(grey.colors(20)), ylim = c(0, 40))
mtext("Year", 1, 2.5, cex = 1.25, font = 2)
mtext("Weight (g)", 2, 2.5, cex = 1.25, font = 2)
box(col = "grey50")

