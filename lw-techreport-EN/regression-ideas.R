##
library(gulf)
# plaice 2020
x <- read.card(year=2020, species=40, card.type="bio")
plot(x$length, x$weight)

keep.idx <- which(x$weight>0 & x$length>10 & x$sex %in% c(1,2))

x <- x[keep.idx,]


## option 1: lm on log10-transformed and ln-transformed values
x$lnlength <- log(x$length)
x$lnweight <- log(x$weight)
x$log10length <- log10(x$length)
x$log10weight <- log10(x$weight)

m1.1 <- lm(log10weight ~ log10length, data=x)
m1.2 <- lm(lnweight ~ lnlength, data=x)

## option 2: same model using GLM
m2.1 <- glm(log10weight ~ log10length, data=x, family=gaussian("identity"))
m2.2 <- glm(lnweight ~ lnlength, data=x, family=gaussian("identity"))

## the beta parameters should be comparable
coef(m1.1)
coef(m1.2)
coef(m2.1)
coef(m2.2)
## and they are
lines(1:50, 10^predict(m1.1, newdata=data.frame(log10length=log10(1:50))))
lines(1:50, exp(predict(m1.2, newdata=data.frame(lnlength=log(1:50)))), col="blue")
lines(1:50, exp(predict(m2.1, newdata=data.frame(log10length=log10(1:50)))), col="red")
lines(1:50, exp(predict(m2.2, newdata=data.frame(lnlength=log(1:50)))), col="green")


## now use a gamma and a log link instead
## option 3: GLM with gamma and log link
m3.1 <- glm(weight ~ lnlength, data=x, family=Gamma(link="log"))

plot(x$length, x$weight)
lines(1:50, exp(predict(m3.1, newdata=data.frame(lnlength=log(1:50)))), col="black")

## add a sex effect
m3.2 <- glm(weight ~ lnlength*sex, data=x, family=Gamma(link="log"))
lines(1:50, exp(predict(m3.2, newdata=data.frame(lnlength=log(1:50), sex=1))), col="blue")
lines(1:50, exp(predict(m3.2, newdata=data.frame(lnlength=log(1:50), sex=2))), col="red")

## add a year effect
x <- read.card(year=c(2019,2020), species=40, card.type="bio")
plot(x$length, x$weight)

keep.idx <- which(x$weight>0 & x$length>10 & x$sex %in% c(1,2))

x <- x[keep.idx,]
x$lnlength <- log(x$length)

m3.3 <- glm(weight ~ lnlength*as.factor(year)*sex, data=x, family=Gamma(link="log"))

##
p.2019.male <- predict(m3.3, newdata=data.frame(year=2019, sex=1, lnlength=log(1:50)))
p.2019.female <- predict(m3.3, newdata=data.frame(year=2019, sex=2, lnlength=log(1:50)))
p.2020.male <- predict(m3.3, newdata=data.frame(year=2020, sex=1, lnlength=log(1:50)))
p.2020.female <- predict(m3.3, newdata=data.frame(year=2020, sex=2, lnlength=log(1:50)))

cbind(p.2019.male, p.2019.female,p.2020.male, p.2020.female)

plot(x$length, x$weight)
lines(1:50, exp(p.2019.male), col="blue")
lines(1:50, exp(p.2020.male), col="blue")
lines(1:50, exp(p.2019.female), col="red")
lines(1:50, exp(p.2020.female), col="red")

## table of estimates
tab.df <- expand.grid(year=c(2019,2020), sex=c(1,2))
c1 <- coef(m3.3)[1]
c2 <- coef(m3.3)[2]
c3 <- coef(m3.3)[3]
c4 <- coef(m3.3)[4]
c5 <- coef(m3.3)[5]
c6 <- coef(m3.3)[6]
c7 <- coef(m3.3)[7]
c8 <- coef(m3.3)[8]
tab.df$alpha <- c(c1, c1+c3, c1+c4, c1+c3+c4+c7)
tab.df$beta <- c(c2, c2+c5, c2+c6, c2+c5+c6+c8)


## now how to generalise this over more years and to other species?
## say for all years for plaice
x <- read.card(species=40, card.type="bio")
x$sex <- ifelse(x$sex==1,1,ifelse(x$sex==2,2,0))
keep.idx <- which(x$weight>0 & x$length>10 & x$length<150 & x$sex %in% c(1,2) & x$year %in% c(1971:2021) & x$stratum %in% c(415:429,431:439))
x <- x[keep.idx,]
x$sex <- factor(x$sex, levels=c(2,1), ordered=TRUE) ## so model estimates are for females and deviations for males and undetermined
x$lnlength <- log(x$length)
table(x$sex)
m3.3 <- glm(weight ~ lnlength*as.factor(year)*sex, data=x[keep.idx,], family=Gamma(link="log"))

coefs <- coef(m3.3)

yearly.a <- c(coefs[1], coefs[1] + coefs[3:52])
yearly.b <- c(coefs[2], coefs[2] + coefs[54:103])

male.a <- c(coefs[1], coefs[1] + coefs[3:52])
female.a <- c(coefs[1], coefs[1] + coefs[3:52] + coefs[53] + coefs[105:154])

male.b <- c(coefs[2], coefs[2] + coefs[54:103])
female.b <- c(coefs[2], coefs[2] + coefs[54:103] + coefs[104] + coefs[155:204])

length(male.a)
length(male.b)
length(female.a)
length(female.b)

plaice.df <- data.frame(cbind(1971:2021,yearly.a,yearly.b,female.a, female.b, male.a, male.b), row.names = 1:51)
dim(plaice.df)
names(plaice.df)[1] <- "year"

plot(yearly.b~year, data=plaice.df, type='b')
grid()
# b estimate for year 1979 are weird


plot(x$length, x$weight)

## predicted weight at 40cms
plot(1971:2021,exp(plaice.df$yearly.a) * 40^plaice.df$yearly.b, type='b')
grid()

idx <- which(x$year==1979)
plot(weight~length, data=x[idx,], col="blue")
idx <- which(x$year==1990)
points(weight~length, data=x[idx,], col="red")


