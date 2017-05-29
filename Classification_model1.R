# Build Neural Network for classification using neuralnet library.
rm(list=ls(all=TRUE))

library(RCurl)
data=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))
#Removing ID,Zipcode

data=subset(data,select = -c(ID,zip)) 

# R NN library takes only numeric attribues as input
str(data)
#Numeric attributes : age,exp,inc,family,CCAvg,
#Mortgage
#Categorical: Education,Securities account,CD Account,Online,
#Credit card
#Target Variable: Personal Loan

num_data=data.frame(sapply(data[c('age','exp','inc','family','ccavg')],function(x){as.numeric(x)}))

#Converting numeric data to similar range
library(vegan)
num_data=decostand(num_data,method = 'range')

#Categorical to numerical
library(dummies)
categ_attributes=c('edu','securities','cd','online')
categ_data=data.frame(sapply(data[categ_attributes],function(x){as.factor(x)}))
categ_data=dummy.data.frame(categ_data,sep="_")

loan=data$loan

#Final data
data=cbind(num_data,categ_data,loan)

set.seed(456) 

#Stratified sampling based on target variable
library(caTools)
index=sample.split(data$loan,SplitRatio = 0.7)
train_Data=data[index,]
test_Data=data[!index,]

#Building neuralnetwork

install.packages("neuralnet")
library(neuralnet)
formula = as.formula(paste("loan~", 
                           paste(setdiff(names(train_Data),"loan"), 
                                 collapse = " + ")))

nn_model = neuralnet(formula,data=train_Data, hidden=3)  

# See covariate and result varaibles of neuralnet model
#covariate tells about the variables that had been used to train the neural network
#net.result[[1]] a matrix containing the overall result of the neural network
result= cbind(nn_model$covariate, nn_model$net.result[[1]])
head(result)

# Ploting the neural network
plot(nn_model)

# Computing confusion matrix and calculating recall on train_Data

predicted = factor(ifelse(nn_model$net.result[[1]] > 0.5, 1, 0))

conf_Matrix = table(train_Data$loan, predicted)
recall = (conf_Matrix[2,2] / (conf_Matrix[2,1] + conf_Matrix[2,2])) * 100
recall

# Removing target attribute from test_Data
test_Data_No_Target = subset(test_Data, select=-c(loan))

# Predict 
nn_predict = compute(nn_model, covariate= test_Data_No_Target)


# Compute confusion matrix and calculate recall for Test Data
predicted = factor(ifelse(nn_predict$net.result > 0.5, 1, 0))
conf_Matrix = table(test_Data$loan, predicted)
recall = (conf_Matrix[2,2]/(conf_Matrix[2,1]+conf_Matrix[2,2]))*100
print(paste('recall is',round(recall,2)))

