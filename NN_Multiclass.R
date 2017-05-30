#Building neural net using iris dataset to demonstrate multi class problem
rm(list = ls(all=TRUE))

#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)

#Loading iris data set and splitting into train and test data
data(iris)
head(iris)
table(iris$Species)
set.seed(123)

library(caTools)
index=sample.split(iris$Species,SplitRatio = 0.8)
train_Data=iris[index,]
test_Data=iris[!index,]
table(train_Data$Species)
table(test_Data$Species)

str(train_Data)

#Converting target variable to factors
final_trainData=cbind(subset(train_Data,select = -Species),class.ind(train_Data$Species))

#Builing model Using neural net library
formula=as.formula(paste(paste(colnames(class.ind(train_Data$Species)),collapse = "+"),"~",
      paste(colnames(subset(train_Data,select = -Species)),collapse = "+")))
nn_model=neuralnet(formula,final_trainData,hidden = 2)

#Plotting neural net model 
plot(nn_model)

summary(nn_model)

#Prediction on train data
target_levels = colnames(class.ind(iris$Species))
predicted=target_levels[max.col(nn_model$net.result[[1]])]
actual = target_levels[max.col(nn_model$response)]

# Computing confusion matrix on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix 
print("Recall on train data is ")
Recall_Sentosa=(conf_Matrix[1,1]/(conf_Matrix[1,1]+conf_Matrix[2,1]+conf_Matrix[3,1]))*100
Recall_versicolor=(conf_Matrix[2,2]/(conf_Matrix[2,2]+conf_Matrix[1,2]+conf_Matrix[3,2]))*100
Recall_virginica=(conf_Matrix[3,3]/(conf_Matrix[3,3]+conf_Matrix[2,3]+conf_Matrix[1,3]))*100


# Remove target attribute from Test Data
test_Data_wo_target = subset(test_Data, select=-c(Species))

# Predicting on test data 
nn_predict = compute(nn_model, covariate= test_Data_wo_target)


# Construct Confusion Matrix 
predicted = target_levels[max.col(nn_predict$net.result)]
actual = test_Data$Species

# Compute confusion matrix and calculate recall on Train Data
conf_Matrix = table(actual, predicted)
conf_Matrix
print("Recall on test data is ")
Recall_Sentosa=(conf_Matrix[1,1]/(conf_Matrix[1,1]+conf_Matrix[2,1]+conf_Matrix[3,1]))*100
Recall_versicolor=(conf_Matrix[2,2]/(conf_Matrix[2,2]+conf_Matrix[1,2]+conf_Matrix[3,2]))*100
Recall_virginica=(conf_Matrix[3,3]/(conf_Matrix[3,3]+conf_Matrix[2,3]+conf_Matrix[1,3]))*100

