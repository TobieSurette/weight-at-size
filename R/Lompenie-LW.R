##
library(gulf)

sp <- c(622,623,624,625,626,631,632)
species.str(sp, as.data.frame=TRUE)

x <- read.card(species=sp, card.type="bio")
idx <- which(!is.na(x$weight) & x$weight<999)

library(ggplot2)

g <- ggplot(data=x[idx,], aes(x=length, y=weight, by=species)) +
  geom_point() +
  facet_grid(vars(species))

g

## save to rda
vars <- c("year","vessel.code","cruise.number","species","length","weight","sex")
x.keep <- x[idx,vars]
save(x.keep, file="Lompenie-LW-RV-1970-2023.csv")

