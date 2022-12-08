# Breast Cancer

View(breast.cancer)

library('car')



#Load the data

cancer <- breast.cancer

str(cancer)


#check null values
sapply(cancer, function(x) sum(is.na(x))) # No null values

# Our target variable is categorical so convert it in factor

cancer$diagnosis <- as.factor(cancer$diagnosis)

str(cancer)

# Column 1 which is id not required so let's remove it

can <- subset(cancer,select=-c(id))
View(can)                 
str(can)
summary(can)

##Model 1 - Classification Tree

#Step 1: Load the necessary packages.
library(rpart) #for fitting decision trees
library(rpart.plot) #for plotting decision trees

#Step 2: Build the initial classification tree.

#build the initial tree
tree <- rpart(diagnosis~., data=can, control=rpart.control(cp=.0001))

#view results
printcp(tree)

#Step 3: Prune the tree.

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

##Model2: Naive Bayes
##Load the necessary packages.

library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)

can$diagnosis <- factor(can$diagnosis, levels = c(0,1), labels = c("M", "B"))

#Building a model
#split data into training and test data sets

indxTrain <- createDataPartition(y = can$diagnosis,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] #Check dimensions of the split > prop.table(table(can$diagnosis)) * 100

prop.table(table(training$diagnosis)) * 100

prop.table(table(testing$Outcome)) * 100

colnames(can)
#create objects x which holds the predictor variables and y which holds the response variables
x = training[-1]
y = training$diagnosis

can$diagnosis <- factor(can$diagnosis, levels = c(0,1), labels = c("B", "M"))
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model

##Cart Model

## cart model
library("rpart")
library(rattle)

cart_model <- train(diagnosis ~ ., train_can, method="rpart")
cart_model

#Model4: Boosted tree

#Loading required package: survival

#Loading required package: splines

# Loading required package: parallel

set.seed(1)
gbm_model <- train(diagnosis ~ ., train_can, method="gbm", verbose=FALSE)

gbm_model

#Testing on testing data

gbm_model$finalModel

#Performance on testing set:

pred5 <- predict(gbm_model, test_data)
confusionMatrix(pred5, test_data$diagnosis, positive="M")

#Accuracy was found to be 97%

#Accuracy Measure
#Boosted Tree: 97% Random Forest : 94% Naive Bayes : 92% CART : 91%
  
#Boosted tree method has given the best accuracy among the four


