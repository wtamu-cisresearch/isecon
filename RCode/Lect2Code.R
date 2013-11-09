#Lect2 Exploratory Data Analysis
data(airquality)
View(airquality)
str(airquality)
summary(airquality)

aq <- na.omit(airquality)
pairs(aq)
pairs(aq[,1:5])
pairs(aq[,1:5], lower.panel=NULL)

attach(aq)
boxplot(Ozone~Month)
plot(Temp, Ozone, col=Month, pch=8)

install.packages("ggplot2")
library(ggplot2)
qplot(Ozone)
qplot(Ozone, binwidth=1)
qplot(Ozone, col=as.factor(Month), binwidth=1)

qplot(Month, Ozone)
qplot(Month, Ozone, size=Wind)
qplot(Month, Ozone, size=Wind, geom="jitter")
qplot(Month, Ozone, size=Wind, geom="jitter",w=0.0, h=0.0)
qplot(Month, Ozone, size=Wind, col=Month)
qplot(Month, Ozone, size=Wind, col=as.factor(Month), geom="jitter")

qplot(Temp, Ozone)
qplot(Temp, Ozone, col=Month, size=Wind)
qplot(Temp, Ozone, col=as.factor(Month), size=Wind)
qplot(Temp, Ozone, col=as.factor(Month), size=Solar.R)
par(mfrow=c(1,2))

#I need a Fifth Dimension
summary(Temp)
tempCut <- cut(Temp , seq(50, 100,by=5))
table(tempCut)
barplot(table(tempCut))
qplot(tempCut)
aq$tempCut <- tempCut
View(aq)

qplot(Wind, Ozone, col=as.factor(Month), size=Solar.R, shape=tempCut)
qplot(Wind, Ozone, col=as.factor(Month), size=Solar.R, shape=tempCut)+
                                    scale_shape_manual(values=1:length(unique(tempCut)))

table(Month)
Month 5 6 7 8 9 31 30 31 31 30
aqMonth56 <- aq[aq$Month== 5 | aq$Month== 6,]
qplot(aqMonth56$Wind, aqMonth56$Ozone, colour=aqMonth56$tempCut, shape=as.factor(aqMonth56$Month), cex=1.5)

#Sampling
sample(2, 20, p=c(0.3, 0.7), replace=T)
sample(1:100, 10)
sample(1:100, 10)
set.seed(12345)
aq10 <- aq[sample(1:nrow(aq), 10),]
aq10

#Subsetting aq[Conditions,]
attach(airquality)
airquality[is.na(Ozone),]
nrow(airquality[is.na(Ozone),])
airquality[is.na(Ozone) & Month==8& (Day==23 | Day== 4),]

#Sorting
airquality[order(Month),]
airquality[order(-Month),]
airquality[order(Month, Day),c(5,6)]

#General Plotting
x<- seq(-4, 4, 0.1)
y <- dnorm(x)
plot(x, y, type="l")  

abline(v=-1.965, col="red", lty=2) #help(abline)
abline(v=0, col="blue", lty=2) 
abline(v=1.965, col="red", lty=2)
segments(-1.965, 0.2, 1.965, 0.2, col="red") #help(segment)

mtext("1.965", side=1, col="red", at=1.965) #help mtext
mtext("-1.965", side=1, col="red", at=-1.965)

