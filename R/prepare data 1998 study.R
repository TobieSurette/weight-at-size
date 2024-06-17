
x <- read.csv("data/ENS1997.csv")
y <- read.csv("data/PEI1986.csv")

plot(as.numeric(y$claw.weight), y$claw.weight.dry)
#iy <- which((y$chela.height - (0.29 * y$carapace.width - 7.5)) > 0)
iy <- which(y$shell.condition == "old")
points(as.numeric(y$claw.weight)[iy],  y$claw.weight.dry[iy], pch = 21, cex = 1, bg = "red")
points(as.numeric(y$claw.weight)[-iy],  y$claw.weight.dry[-iy], pch = 21, cex = 1, bg = "blue")
       
plot(log(y$carapace.width), log(as.numeric(y$claw.weight)))

plot(log(as.numeric(y$claw.weight)) - log(y$claw.weight.dry))

plot(log(as.numeric(y$claw.weight)), log(y$claw.weight.dry))

plot(x$carapace.width, x$carapace.length, cex = 0.2)

plot(x$carapace.width, x$chela.height, ylim = c(0, 45), cex = 0.2)
x$chela.height <- as.numeric(x$chela.height)
ix <- which((x$chela.height - (0.29 * x$carapace.width - 7.5)) > 0)
points(x$carapace.width[ix], x$chela.height[ix], pch = 21, cex = 0.2, bg = "red")
points(x$carapace.width[-ix], x$chela.height[-ix], pch = 21, cex = 0.2, bg = "blue")

abline(-7.5, 0.29)

plot(x$carapace.width, x$merus.length.2, cex = 0.2)
points(x$carapace.width[ix], x$merus.length.2[ix], pch = 21, cex = 0.2, bg = "red")
points(x$carapace.width[-ix], x$merus.length.2[-ix], pch = 21, cex = 0.2, bg = "blue")

plot(x$carapace.width, x$merus.length.3, cex = 0.2)
points(x$carapace.width[ix], x$merus.length.3[ix], pch = 21, cex = 0.2, bg = "red")
points(x$carapace.width[-ix], x$merus.length.3[-ix], pch = 21, cex = 0.2, bg = "blue")

plot(x$merus.length.2, x$merus.width.2, cex = 0.2)
points(x$merus.length.2[ix], x$merus.width.2[ix], pch = 21, cex = 0.2, bg = "red")
points(x$merus.length.2[-ix], x$merus.width.2[-ix], pch = 21, cex = 0.2, bg = "blue")


plot(x$carapace.width, x$weight, cex = 0.2)
points(x$carapace.width[ix], x$weight[ix], pch = 21, cex = 0.2, bg = "red")
points(x$carapace.width[-ix], x$weight[-ix], pch = 21, cex = 0.2, bg = "blue")

plot(x$carapace.width, x$weight, cex = 0.2)

plot(x$carapace.width, x$chela.width, cex = 0.2)
points(x$carapace.width[ix], x$chela.width[ix], pch = 21, cex = 0.2, bg = "red")
points(x$carapace.width[-ix], x$chela.width[-ix], pch = 21, cex = 0.2, bg = "blue")


table(x$shell.condition)

