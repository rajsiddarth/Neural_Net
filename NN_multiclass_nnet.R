#Building neural net for multi class classification using nnet library
rm(list=ls(all=TRUE))

library(RCurl)
data=read.table("https://raw.githubusercontent.com/rajsiddarth/Neural_Net/
                 master/alphabet_features.txt",header=F)

#Target variable is the first column
str(data)
summary(data)

#Normalizing independent variables using decostand
library(vegan)

temp_data=decostand(subset(data,select = -V1),method = "range")
#Adding target variable
data=cbind(data[1],temp_data)

#Converting target variable to factors
data$V1=as.factor(data$V1)
str(data)
table(data$V1)
names(data)[1]="id"

#Dividing into train and test data using stratified sampling
library(caTools)
index=sample.split(data$id,SplitRatio = 0.7)
train_Data=data[index,]
test_Data=data[!index,]

table(train_Data$id)
table(test_Data$id)

#Neural net on train data
library(nnet)
#Hidden nodes=8
set.seed(123)
nn_model=nnet(id ~.,data = train_Data,size=8,rang=0.1,decay=5e-4,maxit=200)

#Predicting on train data
pred_train=predict(nn_model,train_Data,type="class")
conf_Matrix = table(pred_train, train_Data$id)
conf_Matrix = conf_Matrix[order(as.numeric(rownames(conf_Matrix))), ] 
print(paste("Accuracy on train data is =",round((sum(diag(conf_Matrix))/sum(conf_Matrix))*100,2)))

##Predicting on test ata
pred_test=predict(nn_model,test_Data,type="class")
conf_Matrix = table(pred_test, test_Data$id)
conf_Matrix = conf_Matrix[order(as.numeric(rownames(conf_Matrix))), ] 
(sum(diag(conf_Matrix))/sum(conf_Matrix))*100
print(paste("Accuracy on test data is =",round((sum(diag(conf_Matrix))/sum(conf_Matrix))*100,2)))

