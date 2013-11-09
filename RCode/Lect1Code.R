#setwd("E:/CONISAR2013/DataMiningWorkshop/rProject")
getwd()
ls()
rm(list=ls())
help(data)
data()
help(faithful)
data(faithful)
names(faithful)
View(faithful)  #uppercase V
summary(faithful)
class(faithful)
str(faithful)
names(faithful)
head(faithful)
tail(faithful)
nrow(faithful)
ncol(faithful)
length(faithful)
dim(faithful)
faithful[c(3,4,5, 6),]
faithful[c(1:5, 3,4,5, 6),]
faithful$eruptions
faithful[,1]
plot(faithful)
plot(faithful$eruptions, faithful$waiting)

#windows()   #MAC users use quartz()

quartz()
par(mfrow=c(1,2))
plot(table(faithful$eruptions), xlab="E", ylab="C")
plot(table(faithful$waiting), xlab="E", ylab="C")

attach(faithful)
quantile(eruptions)
qn <- seq(0, 1, by=0.1)
qs<- quantile(eruptions, qn)
plot(qs, qn)
plot(qs, qn, type="l")
points(qs, qn, col="red", cex=0.5, pch=8)

install.packages("ggplot2")
library(ggplot2)
windows()
quartz()
data(diamonds)
View(diamonds)
attach(diamonds)
qplot(carat, price)
qplot(carat, price, col=cut)
qplot(carat, price, col=cut, size=table)
boxplot(carat)
boxplot(carat~cut)

par(mfrow=c(1,2))  #sets the drawing area to 1 row by 2 cols
boxplot(diamonds$carat)
boxplot(diamonds$carat~diamonds$cut,col=c(1:length(unique(diamonds$cut))))
