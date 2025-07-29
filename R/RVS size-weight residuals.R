species <- c(40, 41, 42, 43, 143)

species <- c(10, 12, 23, 60)

clg()
png(file = "Weight-at-size residuals fish.png", units = "in", width = 8.5, height = 11, res = 600)
m <- rbind(0, cbind(0, kronecker(t(matrix(1:(2*length(species)), ncol = length(species))), 
                                 matrix(1, ncol = 5, nrow = 5)), 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
for (i in 1:length(species)){
   print(species[i])
   # Read September survey biological data:
   bio <- read.gulf.bio(year = c(1971:2024), species = species[i], password = password)
   bio <- bio[which(bio$length < 100 & bio$length > 0), ]
   bio <- bio[which(bio$weight > 0), ]
   bio <- bio[which(bio$weight < 30000), ]
   bio$year <- year(bio$date)
   bio$sex[!(bio$sex %in% 1:2)] <- NA
   
   model <- lm(log(bio$weight) ~ log(bio$length))
   
   bio$residuals <- log(bio$weight) - (coef(model)[1] + coef(model)[2] * log(bio$length))
   
   for (j in 1:2){
      print(j)
      ix <- bio$sex == j
      t <- aggregate(bio$residuals[ix], by = bio[ix, c("length", "year")], mean)
      
      lens    <- 0:max(bio$length)
      years   <- min(bio$year):max(bio$year)
      res <- matrix(NA, nrow = length(lens), ncol = length(years))
      dimnames(res) <- list(length = lens, year = years)
      for (k in 1:length(years)){
         ix <- which(t[, "year"] == years[k])
         res[as.character(t[ix, 1]), k] <- t[ix, 3]
      }
      #res[res > 0.7] <- NA
      logit <- function(x) log(x / (1-x))
      cols <- colorRampPalette(c("red", "white", "blue"))(50)
      image(years, lens, t(res), col = cols, zlim = c(-0.4, 0.4), 
            xlim = c(min(years)-0.5, max(years)+0.5), xaxs = "i",
            ylim = c(0, 60), yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      grid()

      box(col = "grey50")
      
      if (i == 1){
         mtext(c("Male", "Female")[j], 3, 0.5, font = 2, cex = 1.25)
      }
      
      if ((i == length(species)) & (j == 2)) mtext("Year", 1, 2.75, font = 2, at = min(years), cex = 1.25)
      if ((i == 2) & (j == 1)) mtext("Length (cm)", 2, 2.5, at = 0, font = 2, cex = 1.25)
      if (j == 2) mtext(species(species[i]), 4, 1.55, font = 2, cex = 1.10)
      if (j == 1) if (i == 1) axis(2, at = 10 * (0:6)) else axis(2, at = 10 * (0:5))
      if (i == length(species)) axis(1)
   }
}
dev.off()

#plot(bio$residuals[order(bio$year)], cex = 0.25, ylim = c(-1, 1))


#lines(log(lens), coef(model)[1] + coef(model)[2] * log(lens), col = fade("green", 0.5), lwd = 2)

#boxplot(residuals ~ bio$year, ylim = c(-0.5, 0.5))
#hline(0, col = "red", lwd = 2)

