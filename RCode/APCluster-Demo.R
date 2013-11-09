## load package
library(apcluster)

## create 2D data set
cl1 <- cbind(rnorm(30, 0.3, 0.04), rnorm(30, 0.5, 0.03))
cl2 <- cbind(rnorm(30, 0.7, 0.02), rnorm(30, 0.2, 0.04))
cl3 <- cbind(rnorm(20, 0.50, 0.05), rnorm(20, 0.72, 0.02))
cl4 <- cbind(rnorm(25, 0.50, 0.02), rnorm(25, 0.42, 0.06))
x1 <- rbind(cl1, cl2, cl3, cl4)
plot(x1)

## call apcluster() with default parameters
apres1 <- apcluster(negDistMat(r=2), x1)
apres1
plot(apres1, x1)
heatmap(apres1)

## create another 2D data set with less separated clusters
cl1 <- cbind(rnorm(30, 0.3, 0.05), rnorm(30, 0.7, 0.04))
cl2 <- cbind(rnorm(30, 0.7, 0.04), rnorm(30, 0.4, .05))
cl3 <- cbind(rnorm(20, 0.50, 0.04), rnorm(20, 0.72, 0.03))
cl4 <- cbind(rnorm(25, 0.55, 0.04), rnorm(25, 0.42, 0.04))
x2 <- rbind(cl1, cl2, cl3, cl4)
plot(x2)

## call apcluster() with default parameters
apres2 <- apcluster(negDistMat(r=2), x2)
apres2
plot(apres2, x2)
heatmap(apres2)

## create 4D data set
x3 <- cbind(x1, x2)

## call apcluster() with default parameters
apres3 <- apcluster(negDistMat(r=2), x3)
apres3
plot(apres3, x3[, c(1, 4)])
plot(apres3, x3)
heatmap(apres3)

## create 5D data set with one noise column
x4 <- cbind(x1, runif(nrow(x1), min(x1), max(x1)), x2)

## call apcluster() with default parameters
apres4 <- apcluster(negDistMat(r=2), x4)
apres4
plot(apres4, x4)
heatmap(apres4)

## call apcluster() with q = 0 (p = minimum of similarities)
apres5 <- apcluster(negDistMat(r=2), x4, q=0)
apres5
plot(apres5, x4)
heatmap(apres5)

## run preferenceRange()
preferenceRange(negDistMat(r=2)(x4))
apres6 <- apcluster(negDistMat(r=2), x4, p=-2)
apres6
heatmap(apres6)

## try apclusterK()
apres7 <- apclusterK(negDistMat(r=2), x4, K=5)
apres7
heatmap(apres7)

## use aggExCluster() to join too fine-grained clusterings
aggres1 <- aggExCluster(x=apres4)
plot(aggres1)

## cut at 4 clusters
apres8 <- cutree(aggres1, 4)
apres8
plot(apres8, x4)
heatmap(apres8)

## use non-trivial example with biological sequences
library(kernlab)
data(ch22Promoters)
names(ch22Promoters)[1:5]
substr(ch22Promoters[1:5], 951, 1000)

## compute similarity matrix
promSim <- kernelMatrix(stringdot(length=6, type="spectrum"), ch22Promoters)
rownames(promSim) <- names(ch22Promoters)
colnames(promSim) <- names(ch22Promoters)
promSim[1:5, 1:5]

## run apcluster() with q = 0 (p = minimum of similarities)
promAP <- apcluster(promSim, q=0)
promAP
heatmap(promAP, promSim, Rowv=FALSE, Colv=FALSE)

## run aggExCluster()
promAgg <- aggExCluster(promSim, promAP, includeSim=TRUE)
plot(promAgg)
heatmap(promAgg)

prom5 <- cutree(promAgg, k=5)
prom5
heatmap(prom5, promSim, Rowv=FALSE, Colv=FALSE)

## create large 2D data set
cl1 <- cbind(rnorm(600, 0.3, 0.04), rnorm(600, 0.5, 0.03))
cl2 <- cbind(rnorm(400, 0.7, 0.02), rnorm(400, 0.2, 0.04))
cl3 <- cbind(rnorm(300, 0.50, 0.05), rnorm(300, 0.72, 0.02))
cl4 <- cbind(rnorm(450, 0.50, 0.02), rnorm(450, 0.42, 0.06))
x5 <- rbind(cl1, cl2, cl3, cl4)
plot(x5)
dim(x5)

## call apcluster() with default parameters
apres9 <- apcluster(negDistMat(r=2), x5)
apres9
plot(apres9, x5)
heatmap(apres9)

## run apcluster() with lower input preference
preferenceRange(negDistMat(r=2)(x5))
apres10 <- apcluster(negDistMat(r=2), x5, p=-2)
apres10
plot(apres10, x5)
heatmap(apres10)

## now try apclusterL()
apres11 <- apclusterL(negDistMat(r=2), x5, p=-2, frac=0.05, sweeps=3)
apres11
plot(apres11, x5)
heatmap(apres11)

## now try apclusterL() with an even smaller fraction
apres12 <- apclusterL(negDistMat(r=2), x5, p=-2, frac=0.01, sweeps=3)
apres12
plot(apres12, x5)
heatmap(apres12)
