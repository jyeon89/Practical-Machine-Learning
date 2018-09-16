library(caret)

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

library(randomForest)


# Download the training data
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
              destfile = "./pml-training.csv", method = "curl")

# Load the training dataset
dt_training <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""))

# Download the testing data
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
              destfile = "./pml-testing.csv", method = "curl")

# Load the testing dataset
dt_testing <- read.csv("./pml-testing.csv", na.strings=c("NA","#DIV/0!",""))


features <- names(dt_testing[,colSums(is.na(dt_testing)) == 0])[8:59]

# Only use features used in testing cases.
dt_training <- dt_training[,c(features,"classe")]
dt_testing <- dt_testing[,c(features,"problem_id")]

dim(dt_training); dim(dt_testing);


set.seed(12345)

inTrain <- createDataPartition(dt_training$classe, p=0.6, list=FALSE)
training <- dt_training[inTrain,]
testing <- dt_training[-inTrain,]

dim(training); dim(testing);


modFitDT <- rpart(classe ~ ., data = training, method="class")
fancyRpartPlot(modFitDT)

# Prediction
set.seed(12345)

prediction <- predict(modFitDT, testing, type = "class")
confusionMatrix(prediction, testing$classe)


# Building
set.seed(12345)
modFitRF <- randomForest(classe ~ ., data = training, ntree = 1000)

# Prediction
prediction <- predict(modFitRF, testing, type = "class")
confusionMatrix(prediction, testing$classe)


# Decision Tree Model
predictionDT <- predict(modFitDT, dt_testing, type = "class")
predictionDT

# Random Forest Model
predictionRF <- predict(modFitRF, dt_testing, type = "class")
predictionRF


