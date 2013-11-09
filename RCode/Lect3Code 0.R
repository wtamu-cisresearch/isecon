#Iris Exploratory Data Analysis
##Iris Slide
data(iris)
View(iris)
sampleIris<- iris[sample(1:150, 10),]
sampleIris

##Iris Slide
plot(density(iris$Sepal.Length), ylim=c(0, 1.5), xlab="Sepal.Length", main="",lwd=2)
lines(density(iris[iris$Species=="setosa",]$Sepal.Length), col=2)
lines(density(iris[iris$Species=="versicolor",]$Sepal.Length), col=3)
lines(density(iris[iris$Species=="virginica",]$Sepal.Length), col=4)

text(x=8, y=1.5, label="Setosa", col=2)
text(x=8, y=1.45, label=" versicolor", col=3)
text(x=8, y=1.4, label="virginica", col=4)
text(x=8, y=1.35, label="overall")

##Iris Slide
plot(density(iris$Petal.Length), ylim=c(0, 2.6), xlab="Petal.Length", main="", lwd=1)
lines(density(iris[iris$Species=="setosa",]$Petal.Length), col=2)
lines(density(iris[iris$Species=="versicolor",]$Petal.Length), col=3)
lines(density(iris[iris$Species=="virginica",]$Petal.Length), col=4)

text(x=7, y=2.5, label="Setosa", col=2)
text(x=7, y=2.35, label="versicolor", col=3) 
text(x=7, y=2.2, label="virginica", col=4)
text(x=7, y=2.0, label="Overal")

#Clustering Slides
##Install, Load and Help
install.packages("apcluster")
library(apcluster)
getClassDef("APResult")
getClassDef("AggExResult")

#Clustering the Irises
data(iris)
View(iris)
help(negDistMat)
simMat<- negDistMat(iris[,1:4]) #Default Euclidean
simMat[1:5, 1:5]

simMat2<- negDistMat(iris[,1:4], r=2)
simMat2[1:5, 1:5]
simMat[1:5, 1:5]^2#Basically the Default simMat^2 is simMat2

help(apcluster)
apIris2<- apcluster(simMat2, details=TRUE)
apIris2
str(apIris2)
sDef("APResult")
apIris2@it
apIris2@clusters[[1]]

#Visualizing the Clusters 2-Dims at a Time
help(plot)
plot(apIris2)
windows()
par(mfrow=c(2,2))
plot(apIris2, as.matrix(iris[,c(1,2)]), xlab="SL", ylab="SW")
plot(apIris2, as.matrix(iris[,c(1,3)]), xlab="SL", ylab="PL")
plot(apIris2, as.matrix(iris[,c(1,4)]), xlab="SL", ylab="PW")
plot(apIris2, as.matrix(iris[,c(2,3)]), xlab="SW", ylab="PL")

plot(apIris2, as.matrix(iris[,1:4]))
