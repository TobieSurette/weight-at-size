##
##rm(list=ls())
library(gulf)
species.str(632)
## Stout eelblenny example
## read bio cards
x <- read.card(species=c(632), card.type="bio")
dim(x)

idx <- which(!is.na(x$length) & !is.na(x$weight) & x$length<20 & x$length>0 & x$weight>0)

length(idx)

x <- x[idx,]
table(x$year, x$length)
table(x$year)

library(ggplot2)
g <- ggplot(data=x, aes(x=length,y=weight)) +
  geom_jitter()

g

x$llength <- log(x$length)
x$lweight <- log(x$weight)

lm1 <- lm(lweight~llength, data=x)

plot(log(jitter(x$length, amount = 0.5)), log(jitter(x$weight,amount=0.5)), cex=0.1, pch=19)
abline(a=coef(lm1)[1], b=coef(lm1)[2], col="red")



x$residuals <- (x$lweight-predict(lm1))
boxplot(residuals~llength, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

lm3 <- lm(lweight~llength*as.factor(year), data=x)
boxplot(residuals(lm3)~year, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

boxplot(residuals(lm3)~length, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")


alphas <- coef(lm3)[1] + coef(lm3)[3:25]

betas <- coef(lm3)[2] + coef(lm3)[26:48]

library(lme4)

my.lme.2 <- lmer(lweight~(1|year)+llength,
                 data=x)


my.lme.3 <- lmer(lweight~(llength|year)+llength,
                 data=x)

AIC(my.lme.2)
AIC(my.lme.3)

alphas <- fixef(my.lme.3)[1] + ranef(my.lme.3)$year[,1]
betas <- fixef(my.lme.3)[2] + ranef(my.lme.3)$year[,2]

plot(residuals(my.lme.3))
abline(h=0, col="red")

str(my.lme.3)



