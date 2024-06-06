##
##rm(list=ls())
library(gulf)

## windowpane example
## read bio cards
x <- read.card(species=143, card.type="bio")
dim(x)

idx <- which(!is.na(x$length) & !is.na(x$weight) & x$length<999 & x$length>0 & x$weight>0)

length(idx)

x <- x[idx,]
table(x$year)
table(x$sex,x$year)

x$sex.fac <- as.factor(x$sex)

library(ggplot2)
g <- ggplot(data=x[sample(1:nrow(x),1000,),], aes(x=length,y=weight,col=sex.fac)) +
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

lm2 <- lm(lweight~llength*as.factor(sex), data=x)
boxplot(residuals(lm2)~x$length, ylim=c(-0.5,0.5))
abline(h=0, col="red")


quantile(x[which(x$sex==2),"length"], p=c(0.95,0.98,0.99))

table(x$year, x$length>25)



lm3 <- lm(lweight~llength*as.factor(year), data=x)
boxplot(residuals(lm3)~year, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

boxplot(residuals(lm3)~length, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")


library(lme4)

my.lme.2 <- lmer(lweight~(1|year)+llength,
                 data=x)

my.lme.3 <- lmer(lweight~(llength|year)+llength,
                 data=x)

mycols <- rainbow(length(1970:2023))


plot(c(10,40), exp(c(0,7.0)), type='n')
for(ii in 1:length(1970:2023)){
  # ii <- 1
  alpha <- fixef(my.lme.3)[1] + ranef(my.lme.3)$year[ii,1]
  beta <- fixef(my.lme.3)[2] + ranef(my.lme.3)$year[ii,2]

  lines(10:100, exp(alpha+(beta*log(10:100))), col=mycols[ii])

}

boxplot(residuals(my.lme.3)~x$length)
abline(h=0, col="red")

alphas <- fixef(my.lme.3)[1] + ranef(my.lme.3)$year[,1]
betas <- fixef(my.lme.3)[2] + ranef(my.lme.3)$year[,2]

plot(alphas,betas)

