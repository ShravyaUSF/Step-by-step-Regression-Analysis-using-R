##Shravya Katukuri
#6304 Regression Project
setwd("E:/Sem II/AMB/Project")
rm(list=ls())
library(readxl)
library(car)
midwest=read_excel("6304 Regression Project Data.xlsx")
colnames(midwest) = tolower(make.names(colnames(midwest)))
attach(midwest)
midwest$popcollege=poptotal*percollege/100
midwest$popcollege=round(midwest$popcollege, digits = 0)
midwest$popprof=poptotal*perprof/100
midwest$popprof=round(midwest$popprof, digits = 0)
midwest$ratio=popchild/popadult
midwest$popchildpoverty=popchild*perchildpoverty/100
midwest$popchildpoverty=round(midwest$popchildpoverty, digits = 0)
rural=midwest[which(inmetro==0),]
metropolitan=midwest[which(inmetro==1),]
set.seed(59033316)
random.rural=rural[sample(1:nrow(rural),60,replace=FALSE),]
set.seed(59033316)
random.metropolitan=metropolitan[sample(1:nrow(metropolitan),30,replace=FALSE),]
perelderlypoverty.out=lm(perelderlypoverty~.-id -county -state -inmetro,data=random.rural)
summary(perelderlypoverty.out)
AIC(perelderlypoverty.out)
vif(perelderlypoverty.out)

step(lm(perelderlypoverty~.-id -county -state -inmetro, data=random.rural),direction="both")

perelderlypoverty.out.2=lm(log(perelderlypoverty) ~ area + poptotal + popwhite + 
     popblack + popadult + percollege + perprof + perchildpoverty + 
     popcollege + popprof + ratio + popchildpoverty, data = random.rural)
summary(perelderlypoverty.out.2)
vif(perelderlypoverty.out.2)
AIC(perelderlypoverty.out.2)

perelderlypoverty.out.3=lm(log(perelderlypoverty) ~ area + poptotal + popwhite +
                             popblack + popadult + percollege +perprof + perchildpoverty + 
                             popcollege + I(popcollege^2)+popprof +I(popprof^2)+ratio + popchildpoverty, data = random.rural)

summary(perelderlypoverty.out.3)
vif(perelderlypoverty.out.3)
AIC(perelderlypoverty.out.3)

perelderlypoverty.out.4=lm(log(perelderlypoverty) ~ area + 
                             popblack + percollege +perprof + perchildpoverty + 
                             popcollege + I(popcollege^2)+popprof +I(popprof^2)+ratio + popchildpoverty, data = random.rural)

summary(perelderlypoverty.out.4)
vif(perelderlypoverty.out.4)
AIC(perelderlypoverty.out.4)


perelderlypoverty.out.5=lm(log(perelderlypoverty) ~ area + 
                             percollege + perchildpoverty + 
                             popcollege + I(popcollege^2)+popprof +I(popprof^2)+ratio, data = random.rural)

summary(perelderlypoverty.out.5)
vif(perelderlypoverty.out.5)
AIC(perelderlypoverty.out.5)

hist(random.rural$popchildpoverty)


qqnorm(perelderlypoverty.out.5$residuals,pch=19)
qqline(perelderlypoverty.out.5$residuals,col="red",lwd=3)
stdresid=rstandard(perelderlypoverty.out.5)
plot(random.rural$perelderlypoverty,stdresid, pch=19,xlab = "perelderlypoverty", ylab = "std residuals")
abline(0,0,col="red",lwd=3)
par(mfrow=c(2,2))
plot(perelderlypoverty.out.5)

hist(perelderlypoverty)
hist(log(perelderlypoverty))

m.num = random.rural[c(4,12,14,18,19)]
View(m.num)
cor(m.num)

qqnorm(perelderlypoverty.out.3$residuals,pch=19)
qqline(perelderlypoverty.out.3$residuals,col="red",lwd=3)
stdresid=rstandard(perelderlypoverty.out.3)
plot(random.rural$perelderlypoverty,stdresid, pch=19,xlab = "perelderlypoverty", ylab = "std residuals")
abline(0,0,col="red",lwd=3)
par(mfrow=c(2,2))
plot(perelderlypoverty.out.3)

plot(perelderlypoverty~perelderlypoverty.out.3$fitted.values, pch=19)

shapiro.test(perelderlypoverty.out.3$residuals)
bartlett.test(list(perelderlypoverty.out.3$residuals, perelderlypoverty.out.3$fitted.values))


lev=hat(model.matrix(perelderlypoverty.out.5))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
random.rural[lev>(3*mean(lev)),]

perelderlypoverty.out.6=lm(perelderlypoverty ~ area + 
                             percollege + perchildpoverty + 
                             popcollege + I(popcollege^2)+popprof +I(popprof^2)+ratio, data = random.metropolitan)

summary(perelderlypoverty.out.6)
vif(perelderlypoverty.out.6)
AIC(perelderlypoverty.out.6)

qqnorm(perelderlypoverty.out.6$residuals,pch=19)
qqline(perelderlypoverty.out.6$residuals,col="red",lwd=3)
stdresid=rstandard(perelderlypoverty.out.6)
plot(random.rural$perelderlypoverty,stdresid, pch=19,xlab = "perelderlypoverty", ylab = "std residuals")
abline(0,0,col="red",lwd=3)
par(mfrow=c(2,2))
plot(perelderlypoverty.out.6)
shapiro.test(perelderlypoverty.out.5$residuals)
bartlett.test(list(perelderlypoverty.out.5$residuals,perelderlypoverty.out.5$fitted.values))

