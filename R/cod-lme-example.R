##
##
library(gulf)

## cod example
## read bio cards
x <- read.card(species=10, card.type="bio")
dim(x)

idx <- which(!is.na(x$length) & !is.na(x$weight) & x$length<999 & x$length>0 & x$weight>0)

length(idx)

x <- x[idx,]

x[which(x$length>100 & x$weight<5000),]
plot(jitter(x$length, amount = 0.5), x$weight, cex=0.1, pch=19)

plot(log(jitter(x$length, amount = 0.5)), log(jitter(x$weight,amount=0.5)), cex=0.1, pch=19)


x$llength <- log(x$length)
x$lweight <- log(x$weight)

lm1 <- lm(lweight~llength, data=x)

plot(log(jitter(x$length, amount = 0.5)), log(jitter(x$weight,amount=0.5)), cex=0.1, pch=19)
abline(a=coef(lm1)[1], b=coef(lm1)[2], col="red")

x$residuals <- (x$lweight-predict(lm1))
boxplot(residuals~llength, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

boxplot(residuals~year, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

boxplot(residuals~stratum, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

boxplot(residuals~sex, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")


lm2 <- lm(lweight~llength+as.factor(year), data=x)
summary(lm2)
boxplot(residuals(lm2)~year, data=x, ylim=c(-0.5,0.5))
abline(h=0, col="red")

n.df <- data.frame(
  year=as.factor(1970:2023),
  llength=log(50)
)
n.df$pred.lm2 <- exp(predict(lm2, newdata=n.df))
plot(pred.lm2~year, data=n.df, pch=19, type='b')

idx <- which(x$length==50)
agg <- aggregate(weight~year, x[idx,], mean)
lines(n.df$year, agg$weight)


est.df2 <- data.frame(
  year=1970:2023,
  alpha=c(coef(lm2)[1], coef(lm2)[1]+coef(lm3)[c(3:55)]),
  beta=c(coef(lm2)[2])
)



lm3 <- lm(lweight~llength*as.factor(year), data=x)
summary(lm3)
anova(lm2,lm3)

n.df$pred.lm3 <- exp(predict(lm3, newdata=n.df))
plot(pred.lm3~year, data=n.df, pch=19, type='b')
lines(pred.lm2~year, data=n.df, pch=19, type='b')
lines(n.df$year, agg$weight,col="red")


AIC(lm2)
AIC(lm3)

est.df <- data.frame(
year=1970:2023,
  alpha=c(coef(lm3)[1], coef(lm3)[1]+coef(lm3)[c(3:55)]),
beta=c(coef(lm3)[2], coef(lm3)[2]+coef(lm3)[c(56:108)])
)
plot(alpha~year, data=est.df,type='b',pch=19)
plot(beta~year, data=est.df,type='b',pch=19)


idx.1979 <- which(x$year==1979)
plot(x[idx.1979,"llength"],x[idx.1979,"lweight"])
idx.1980 <- which(x$year==1980)
points(x[idx.1980,"llength"],x[idx.1980,"lweight"], col="red",pch=19)


for(ll in 10:100){
  #ll<-10
  temp <- data.frame(est.df$alpha + (est.df$beta*log(ll)))

  names(temp) <- paste0("pred.",ll)
  est.df <- cbind(est.df, temp)
}

vars <- paste0("pred.",10:100)
image(as.matrix(est.df[vars]))


plot(log(c(10,100)), c(0,10), type='n')
for(ii in 1:nrow(est.df)){
  #ii<-1
  lines(log(10:100), est.df[ii,vars])
}

mycols <- rainbow(nrow(est.df))
plot(c(10,100), exp(c(0,9.5)), type='n')
for(ii in 1:nrow(est.df)){
  #ii<-1
  lines(10:100, exp(est.df[ii,vars]),col=mycols[ii])
}


library(lme4)
my.lme.2 <- lmer(lweight~(1|year)+llength,
                 data=x)
summary(my.lme.2)
ranef(my.lme.2)
coef(my.lme.2)

plot(coef(my.lme.2)$year[,1], est.df2$alpha)

my.lme.3 <- lmer(lweight~(llength|year)+llength,
                 data=x)
summary(my.lme.3)
ranef(my.lme.3)

plot(c(10,100), exp(c(0,9.5)), type='n')
for(ii in 1:length(1970:2023)){
  # ii <- 1
  alpha <- fixef(my.lme.3)[1] + ranef(my.lme.3)$year[ii,1]
beta <- fixef(my.lme.3)[2] + ranef(my.lme.3)$year[ii,2]

lines(10:100, exp(alpha+(beta*log(10:100))), col=mycols[ii])

}

