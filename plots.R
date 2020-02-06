step(lm(perelderlypoverty~.-id -county -state -inmetro, data=random.rural),direction="both")

m1<-lm(formula = perelderlypoverty ~ area + poptotal + popwhite + 
     popblack + popadult + percollege + perprof + perchildpoverty + 
     popcollege + popprof + ratio + popchildpoverty, data = random.rural)
summary(m1)
vif(m1)
AIC(m1)

m2<-lm(formula = perelderlypoverty ~ area + 
         popblack + percollege + perprof + perchildpoverty + ratio -id -state -county -inmetro, data = random.rural)
summary(m2)
vif(m2)
AIC(m2)

m3<-lm(formula = perelderlypoverty ~ area + 
         popblack + percollege + perprof + perchildpoverty + ratio + popchildpoverty -id -state -county -inmetro, data = random.rural)
summary(m3)
vif(m3)
AIC(m3)

m4<-lm(formula = perelderlypoverty ~ area + popwhite+
         popblack + percollege + perprof + perchildpoverty + ratio + popchildpoverty -id -state -county -inmetro, data = random.rural)
summary(m4)
vif(m4)
AIC(m4)

m5<-lm(formula = perelderlypoverty ~ area + popwhite+
         popblack + percollege + perprof + perchildpoverty + popchildpoverty -id -state -county -inmetro, data = random.rural)
summary(m5)
vif(m5)
AIC(m5)

m6<-lm(perelderlypoverty~popwhite+perchildpoverty - 
         id - state - county - inmetro, data = random.rural)
summary(m6)
vif(m6)
AIC(m6)

install.packages("xlsx")
install.packages(rJava)
library(rJava)
library(xlsx)

install.packages("javax64")
library(javax64)
install.packages("rJava")
library(rJava)
sessionInfo()
Sys.setenv(JAVA_HOME="C:/Program Files/Java/")

install.packages("rJava")
library(rJava)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_91/")
install.packages("rJava")
library(rJava)
library("xlsx")
write.xlsx(random.rural,"C:/Users/SHRAVYA/Desktop/random.rural.xlsx")
