
library(caret)
library(ipred)

#load the Human Activity dataset from Groupware LES:
trainraw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testraw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#subset to remove unnecessary descriptive columns at the front of the data set
training <- trainraw[,c(8:160)]
testing <- testraw[,c(8:160)]

#NA or missing values will make 

#determine which columns contain NAs
trainuse <- sapply(training, function(x)any(is.na(x)))

#remove columns found to contain NAs
training <- training[,as.integer(trainuse) == 0]
testing <- testing[,as.integer(trainuse) == 0]

#determine which columns contain missing values
trainuse <- sapply(training, function(x)any(x==""))

#remove columns found to contain missing values
training <- training[,as.integer(trainuse) == 0]
testing <- testing[,as.integer(trainuse) == 0]

#training <- data.frame(sapply(training[1:52],as.numeric))
#testing <- data.frame(sapply(testing[1:52],as.numeric))

set.seed(294)
#sample 20% of data set for testing
TrainSmall <- createDataPartition(training$classe, p=0.2, list = FALSE)
TrainFinal <- training[TrainSmall,]

#pre-processing steps using Principal Component Analysis
preProc <- preProcess(TrainFinal[,1:52],method="pca")
TrainingProcessed <- predict(preProc,TrainFinal[,1:52])


#training with cross-validation
TrainFit <- trainControl(allowParallel = TRUE, method = "cv", number = 5);
FinalTrainFit <- train(classe ~ ., data = TrainFinal, method="rf",
                  trainControl = TrainFit, importance=TRUE);

plot(varImp(FinalTrainFit))

#test set held out previously, 
TestFinal <- training[-TrainSmall,]
confusionMatrix(TestFinal$classe, predict(modelFit,TestFinal))$table




#Final test set predictions
testing <- testing[,1:52]
predict(modelFit,testing)



confusionMatrix(TestFinal$classe, predict(modelFit,TestFinal))

#The accuracy is 91.5%, so the out of sample error is 8.5%.


#--------------------
