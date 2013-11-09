#Lecture 4 Classification and Prediction with party
install.packages("party")
library(party)
getClassDef("BinaryTree")
x<- new("BinaryTree")
str(x)
help(ctree)

#The Iris Data Set
data(iris)
View(iris)
unique(iris$Species)
table(iris$Species)
irisct<- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris) #OR
irisct<- ctree(Species~., data=iris)  #produces the same result
irisct
str(irisct)
windows()
quartz()
plot(irisct)

#Checking th eModel
predict(irisct)
iris$predictedSpecies<- predict(irisct)
View(iris)
table(iris$Species, iris$predictedSpecies)
where(irisct)     #Where in the terminal nodes do they fall
unique(where(irisct))

nodes(irisct, 1)    #Display Node 1 Info
nodes(irisct, unique(where(irisct)))
for(node in nodes(irisct, unique(where(irisct)))){ print(str(node))}

nodeCount<- max(unique(where(irisct))) 
for(i in 1:nodeCount) {show(nodes(irisct, i))}

irisct@tree
irisct@tree$right
irisct@tree$right$right
irisct@tree$right$left

#A Complete  Preediction Session with training Set and test set
set.seed(1234)  #To Allow for replication of Results
index<-sample(2, nrow(iris), replace=TRUE, prob=c(0.85, 0.15)) 
index       #create 150 records of 1, 2 with 1 Occuring 85%, 2 Occuring 15% percent of the time
table(index)

trainIndex <- index==1 #Convert index=1 to boolean TRUE
trainIndex
testIndex <- index==2  #Convert index=2 to boolean TRUE
testIndex
trainIris<- iris[trainIndex,] #Select the train Dataset
trainIris
nrow(trainIris)
testIris<- iris[testIndex,]  #Select the test Dataset
testIris
nrow(testIris)

#Build the Decision Tree Model over the training data set.
irisCtModel <- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=trainIris) 

quartz()
windows()
plot(irisCtModel) # Getting an Error Plot the Model Tree
table(predict(irisCtModel), trainIris$Species) #Validate the Model    

testPred <- predict(irisCtModel, newdata=testIris[,1:4]) #Run the Text Validation of the Model
table(testPred, testIris$Species) #Validate the Model on Test Dataset

#Perform Actual Predictions
#Create a Record to Test
newRecord <- as.data.frame(list(4.9, 3.0, 1.4, 0.2)) 
newRecord

#Rename the column Names to match the ctree() Model Names
names(newRecord) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
newRecord
predict(irisCtModel, newdata=newRecord)

newRecord <- as.data.frame(list(10, 6.0, 2, 1))
names(newRecord) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
predict(irisCtModel, newdata=newRecord)

attach(iris)
newRecord <- as.data.frame(list(median(Sepal.Length), 
                                median(Sepal.Width),
                                median(Petal.Length), 
                                median(Petal.Width))) 
newRecord
names(newRecord) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
predict(irisCtModel, newdata=newRecord)
##Done