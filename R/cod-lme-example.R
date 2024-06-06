##
##
library(gulf)

## cod example
## read bio cards
x <- read.card(species=10, card.type="bio")
dim(x)

idx <- which(!is.na(x$length) & !is.na(x$weight) & x$length<999 & x$length>0 & x$weight>0 & x$sex %in% c(1,2))

length(idx)

x <- x[idx,]


x$sex <- ifelse(x$sex %in% c(0,9), 9, x$sex)

## average weight-at-length over time

library(ggplot2)
g <- ggplot(data=x, aes(x=length,y=weight,col=sex)) +
  geom_point()
g

my.agg <- aggregate(weight~length+year, x, mean)

g <- ggplot(data=my.agg[which(my.agg$length%in%c(50)),], aes(x=year,y=weight,group=length)) +
  geom_point() + geom_line()
g

g <- ggplot(data=x[which(x$length%in%c(30)),], aes(x=year,y=weight,group=year)) +
  geom_boxplot() + facet_grid(rows=vars(length), scales="free") +
  ylim(150,350)
g


x$year <- factor(x$year, levels=sort(unique(x$year)))#,ordered=TRUE
x$sex <- factor(x$sex, levels=sort(unique(x$sex)))#,ordered=TRUE

x$llength <- log(x$length)
x$lweight <- log(x$weight)

plot(x$llength, x$lweight)
abline(a=-5,b=3, col="red")

library(lme4)
my.lm <- lm(lweight~llength, data=x)
# my.lm <- lm(lweight~llength+year, data=x) ## nonsense

my.lme.1 <- lmer(lweight~(1|year)+llength,
               data=x)
summary(my.lme.1)

x$resids <- residuals(my.lme.1, type="pearson")

idx <- which(abs(x$resids)>2)
x[idx,]

boxplot(resids~year, data=x)

my.lme.2 <- lmer(lweight~llength+sex+(llength|year),
               data=x,
               subset=year %in% c(1996,2000,2004,2023)
               )


summary(my.lme.2)
ranef(my.lme.2)


out.df <- data.frame(
  species=10,
  size.type="fork length",
  size.unit="centimeter",
  weight.unit="kilogram")



