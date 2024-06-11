
x <- read.csv("snow crab/data/size.weight.study 97-98.csv")


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

plot(x$merus.length.2, x$merus.width.2, cex = 0.2)

plot(x$merus.length.3, x$merus.width.3, cex = 0.2)

table(x$shell.condition)

