#Iris Exploratory Data Analysis
##Iris Slide
data(iris)
View(iris)
sampleIris<- iris[sample(1:150, 10),]
sampleIris

##Iris Slide Density Plots
plot(density(iris$Sepal.Length), ylim=c(0, 1.5), xlab="Sepal.Length", main="",lwd=2)
lines(density(iris[iris$Species=="setosa",]$Sepal.Length), col=2)
lines(density(iris[iris$Species=="versicolor",]$Sepal.Length), col=3)
lines(density(iris[iris$Species=="virginica",]$Sepal.Length), col=4)

text(x=8, y=1.5, label="Setosa", col=2)
text(x=8, y=1.45, label=" versicolor", col=3)
text(x=8, y=1.4, label="virginica", col=4)
text(x=8, y=1.35, label="overall")

##Iris Slide Density Plots
plot(density(iris$Petal.Length), ylim=c(0, 2.6), xlab="Petal.Length", main="", lwd=1)
lines(density(iris[iris$Species=="setosa",]$Petal.Length), col=2)
lines(density(iris[iris$Species=="versicolor",]$Petal.Length), col=3)
lines(density(iris[iris$Species=="virginica",]$Petal.Length), col=4)

text(x=7, y=2.5, label="Setosa", col=2)
text(x=7, y=2.35, label="versicolor", col=3) 
text(x=7, y=2.2, label="virginica", col=4)
text(x=7, y=2.0, label="Overal")

#Clustering Slides Prepare the Environment
##Install, Load and Help
#install.packages("apcluster")
library(apcluster)
getClassDef("APResult")
getClassDef("AggExResult")

#Clustering the Irises
data(iris)
View(iris)
help(negDistMat)

#Generate the Negative Similarity Matrix
simMat<- negDistMat(iris[,1:4]) #Default Euclidean
simMat[1:5, 1:5]

simMat2<- negDistMat(iris[,1:4], r=2)
simMat2[1:5, 1:5]
simMat[1:5, 1:5]^2#Basically the Default simMat^2 is simMat2

# Understanding the Results APResult object
help(apcluster)
apIris2<- apcluster(simMat2, details=TRUE)
apIris2
str(apIris2)

#To understand the Structure of APResult
getClassDef("APResult")
x <- new("APResult")
str(x)

apIris2@it
plot(apIris2)
apIris2@clusters[[1]]

#Visualizing the Clusters 2-Dims at a Time
help(plot)
plot(apIris2)
windows()
quartz()
par(mfrow=c(2,2))
plot(apIris2, as.matrix(iris[,c(1,2)]), xlab="SL", ylab="SW")
plot(apIris2, as.matrix(iris[,c(1,3)]), xlab="SL", ylab="PL")
plot(apIris2, as.matrix(iris[,c(1,4)]), xlab="SL", ylab="PW")
plot(apIris2, as.matrix(iris[,c(2,3)]), xlab="SW", ylab="PL")

#Visualizing the Clusters in pairs
plot(apIris2, as.matrix(iris[,1:4]))

#Goodness of Fit superimpose the setos(s) on top of the cluster
windows()
quartz()
par(mfrow=c(1,1))
plot(apIris2, as.matrix(iris[,c(1,2)]), xlab="SL", ylab="SW")
#Superimpose the setosa(s)
points(iris[iris$Species=="setosa",1], iris[iris$Species=="setosa",2], pch=8)
#Superimpose the versicolor(s)
points(iris[iris$Species=="versicolor",1], iris[iris$Species=="versicolor",2],pch=19, col="black")
#Superimpose the virginica(s)
points(iris[iris$Species=="virginica",1], iris[iris$Species=="virginica",2],pch=15, col="red")

legend(6.5, 4.5, legend="setosa", pch=8, box.col="white")
legend(6.5, 4.35, legend="versicolor", pch=19, box.col="white", col="black")
legend(6.5, 4.20, legend="virginica", pch=15, box.col="white", col="red")

#######apclusterK()
apirisK <- apclusterK(simMat2, details=TRUE, K=2, verbose=TRUE)
windows()
quartz()
plot(apirisK, iris[,1:4])#Overall Pairwise
windows()
quartz()
plot(apirisK, as.matrix(iris[, 1:2]), xlab=names(iris)[1], ylab=names(iris)[2])

#Plot Sepal.Length Vs. Sepal.Width
#Superimpose the setosa(s)
points(iris[iris$Species=="setosa",1], iris[iris$Species=="setosa",2], pch=8)
#Superimpose the versicolor(s)
points(iris[iris$Species=="versicolor",1], iris[iris$Species=="versicolor",2],pch=19, col="black")
#Superimpose the virginica(s)
points(iris[iris$Species=="virginica",1], iris[iris$Species=="virginica",2],pch=15, col="red")

legend(6.5, 4.5, legend="setosa", pch=8, box.col="white")
legend(6.5, 4.35, legend="versicolor", pch=19, box.col="white", col="black")
legend(6.5, 4.20, legend="virginica", pch=15, box.col="white", col="red")
#
#

#######apclusterK() with K=3
apirisK <- apclusterK(simMat2, details=TRUE, K=3, verbose=TRUE)
quartz()
windows()
plot(apirisK, iris[,1:4])#Overall Pairwise
quartz()
windows()
plot(apirisK, as.matrix(iris[, 1:2]), xlab=names(iris)[1], ylab=names(iris)[2])

#Plot Sepal.Length Vs. Sepal.Width
#Superimpose the setosa(s)
points(iris[iris$Species=="setosa",1], iris[iris$Species=="setosa",2], pch=8)
#Superimpose the versicolor(s)
points(iris[iris$Species=="versicolor",1], iris[iris$Species=="versicolor",2],pch=19, col="black")
#Superimpose the virginica(s)
points(iris[iris$Species=="virginica",1], iris[iris$Species=="virginica",2],pch=15, col="red")

legend(6.5, 4.5, legend="setosa", pch=8, box.col="white")
legend(6.5, 4.35, legend="versicolor", pch=19, box.col="white", col="black")
legend(6.5, 4.20, legend="virginica", pch=15, box.col="white", col="red")
#
#
#######aggExCluster
help(aggExCluster)
apIrisAgg <- aggExCluster(simMat2)
windows()
quartz()
plot(apIrisAgg)
apIrisAggK3<- cutree(apIrisAgg, k=3)
windows()
quartz()
plot(apIrisAggK3, iris[,1:4])#Overall Pairwise
quartz()
windows()
plot(apIrisAggK3, as.matrix(iris[, 1:2]), xlab=names(iris)[1], ylab=names(iris)[2])
#Plot Sepal.Length Vs. Sepal.Width
#Superimpose the setosa(s)
points(iris[iris$Species=="setosa",1], iris[iris$Species=="setosa",2], pch=8)
#Superimpose the versicolor(s)
points(iris[iris$Species=="versicolor",1], iris[iris$Species=="versicolor",2],pch=19, col="black")
#Superimpose the virginica(s)
points(iris[iris$Species=="virginica",1], iris[iris$Species=="virginica",2],pch=15, col="red")

legend(6.5, 4.5, legend="setosa", pch=8, box.col="white")
legend(6.5, 4.35, legend="versicolor", pch=19, box.col="white", col="black")
legend(6.5, 4.20, legend="virginica", pch=15, box.col="white", col="red")



apirises<- iris
apirises$Exemplar<- labels(apirisK, type="exemplars")
apirises$Cluster<- labels(apirisK, type="enum")
write.csv(apirises, "./data/apirisesk3.csv")
View(apiriesk3)

#How about the Faithful Data Set
data(faithful)
View(faithful)
plot(faithful)
x<-c()
for(i in 1: nrow(faithful)){
  if(faithful$eruptions[i] > 3){x<- append(x, 1)}
  else{x<- append(x, 0)}
}
x
faithful$class <- x
View(faithful)
sMF<- negDistMat(faithful[,1:2])
View(sMF)
apFaithful<- apcluster(sMF, details=TRUE)
apFaithfulK<- apclusterK(sMF, details=TRUE, K=2, verbose=TRUE)
plot(apFaithfulK,faithful[,1:2] )
low<- faithful[faithful$class==0, 1:2]
points(low, pch=19, col="black")
high<- faithful[faithful$class==1, 1:2]
points(high, pch=8, col="black")
###################################################