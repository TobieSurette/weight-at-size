library(gulf.data)
library(gulf.graphics)

# Load RV data:
bio <- read.gulf.bio(year = 2012:2017, species = 2526, password = password)
bio <- bio[which((bio$sex == 2) & !is.na(bio$length) & (bio$weight > 0)), ]

# Remove damaged crab:
ix <- grep("damag", tolower(bio$comment))
ix <- c(ix, grep("crush", tolower(bio$comment)))
ix <- c(ix, grep("smash", tolower(bio$comment)))
ix <- c(ix, grep("miss", tolower(bio$comment)))
ix <- c(ix, grep("miss", tolower(bio$comment)))
ix <- c(ix, grep("mssi", tolower(bio$comment)))
ix <- c(ix, grep("broke", tolower(bio$comment))) 
ix <- c(ix, grep("manque", tolower(bio$comment))) 
ix <- c(ix, grep("abdom", tolower(bio$comment))) 
ix <- c(ix, grep("flapp", tolower(bio$comment))) 
ix <- c(ix, grep("pouch", tolower(bio$comment))) 
ix <- c(ix, grep("barn", tolower(bio$comment))) 
bio <- bio[-ix, ]

# Define variables:
bio$carapace.width <- bio$length
bio <- bio[which(bio$carapace.width <= 90), ]
bio$julian <- julian(date(bio))

# Fix maturity:
bio$maturity[grep("maturity code = 2", bio$comment)] <- 2
bio <- bio[!is.na(bio$maturity), ]
bio$maturity[which((bio$carapace.width >= 75) & (bio$maturity == 1))] <- 2
bio$maturity[which((bio$carapace.width <= 37) & (bio$maturity == 2))] <- 1
bio <- bio[which(bio$maturity %in% 1:2), ]

# Remove period in 2014 where no missing leg info was recorded:
ix <- which((year(bio$date) == 2014) & (julian(date(bio$date)) >= 263))
bio <- bio[-ix, ]

# Convert comments to missing leg pattern:
ix <- which(is.na(bio$missing.legs))
bio$missing.legs[ix] <- missing.legs(bio$comment[ix])
bio <- bio[which(nchar(bio$missing.legs) == 10), ]

# Remove problematic weight data:
plot(log(jitter(bio$carapace.width, amount = 0.5)), 
     log(jitter(bio$weight, amount = 0.5)), pch = 21, 
     bg = "grey", cex = 0.2, xlim = c(2, 4.5), ylim = c(0, 6))
abline(-7.2, 3, lty = "dashed", col = "red", lwd = 2)
abline(-9.1, 3, lty = "dashed", col = "red", lwd = 2)
ix <- which((log(bio$weight) - (3 * log(bio$carapace.width) - 7.2)) > 0)
iy <- which((log(bio$weight) - (3 * log(bio$carapace.width) - 9.1)) < 0)
points(log(jitter(bio$carapace.width, amount = 0.5))[c(ix,iy)], 
       log(jitter(bio$weight, amount = 0.5))[c(ix,iy)], col = "red")

bio <- bio[-c(ix, iy), ]


gbarplot(table(bio$carapace.width[bio$maturity == 1]))
gbarplot(table(bio$carapace.width[bio$maturity == 2]))

plot(log(jitter(bio$length, amount = 0.5)), log(jitter(bio$weight, amount = 0.5)), xlim = c(3, 4.5), ylim = c(1, 5.5), cex = 0.2)
points(log(jitter(bio$length, amount = 0.5))[bio$maturity == 1], 
       log(jitter(bio$weight, amount = 0.5))[bio$maturity == 1], pch = 21, bg = "red", cex = 0.2)
points(log(jitter(bio$length, amount = 0.5))[bio$maturity == 2], 
       log(jitter(bio$weight, amount = 0.5))[bio$maturity == 2], pch = 21, bg = "green3", cex = 0.2)


# Parse missing leg pattern:
M <- missing.legs(bio$missing.legs)
R <- M == 2
M <- M == 1

M <- M[,1:5] + M[,6:10]

clc()
iy <- which(apply(R, 1, sum) == 0)
file <- "data/RVS weight data females.txt"
cat("list(\n", file = file)
cat(paste0("n = ", length(iy), ", \n"), file = file, append = TRUE)
cat(paste0("theta.outlier = c(1,1), \n"), file = file, append = TRUE)
cat("log.x = c(", paste0(round(log(bio$carapace.width[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("log.w = c(", paste0(round(log(bio$weight[iy]),3), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("maturity = c(", paste0(round(bio$maturity[iy]-1), collapse = ","), "),\n\n", file = file, append = TRUE)
#cat("hardness = c(", paste0(round((bio$shell.condition[iy] >= 3)), collapse = ","), "),\n\n", file = file, append = TRUE)
cat("missing = structure(.Data = c(\n", file = file, append = TRUE)
cat("    ", paste0(paste0(as.numeric(t(M[iy, ])), collapse = ",")), file = file, append = TRUE)
cat("), \n    .Dim = ", paste0("c(", length(iy), ",", 5, "))\n"), file = file, append = TRUE)
cat(")", file = file, append = TRUE)


