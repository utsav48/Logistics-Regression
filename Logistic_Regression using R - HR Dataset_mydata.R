setwd("E:\\Jigshaw lectures\\R STUDIO CLASS\\regression")

#-------Importing the data---------
#setwd("E:\\Jaishree\\Data Science with R\\Datasets")
hr<-read.csv("HR dataset for Employee Attrition.csv")

# Data Exploration

dim(hr)

str(hr)
View(hr)

# Convert Attrition(target variable) to numeric variable

hr$AttritionTarget <- as.numeric(hr$Attrition)-1
#or hr$AttritionTarget_1<-ifelse(hr$Attrition == "Yes",1,0)
View(hr)

# What is the ratio of Attrited vs. not Attritted?

# Frequency Distribution of the binary dependent variable 

table(hr$AttritionTarget)
table(hr$AttritionTarget)/nrow(hr)

#Checking for missing values in all the columns

colSums(is.na(hr))
sum(is.na(hr))
# No missing values

# Partition the dataset into training and validation dataset  #MAX NO. OF ROWS THEN DIVIDE (TRAIN & TEST)

sampling<-sort(sample(nrow(hr), nrow(hr)*.7))
1470*0.7

length(sampling)

#Row subset and create training and validation samples using the index numbers

train<-hr[sampling,]
test<-hr[-sampling,]
nrow(train)
nrow(test)

# Checking the frequency Distribution of the target variable 

table(train$AttritionTarget)
table(train$AttritionTarget)/nrow(train)
table(test$AttritionTarget)/nrow(test)

#Renaming Age column
class(train)
colnames(train)
colnames(train)[1]<- "AGE"
colnames(train)
#names(train)[1] <- "Age"
colnames(train)
names(test)[1] <- "AGE"

#Are any of the independent variables correlated?  #MULTICOLLINEARITY IF IT IS THERE DROP

install.packages("corrplot", dependencies = T)
library(corrplot)

#Finding correlation between numeric variables 

str(train)
traincor<-cor(train[,c(1,4,6,7,11,13,14,15,17,19,20,21,24,25,26,28:35)])     # take continuos variable(not discrete) 
class(traincor)    #when we do coorelation data frame is converted into matrix
library(corrgram)
?corrgram
cormat<-corrgram(traincor)    

write.csv(cormat,"Correlation.csv")

# After Conditional formatting, we find :
# High correlation between:
# Job Level and Monthly Income
# Job Level and Total Working Years
# Monthly Income and Total Working Years
# Percent Salary Hike and Performance Rating

str(train)
colnames(train)

?glm()
# Family of dependent variable is binary or binomial 
#1,4,6,7,11,13,14,15,17,19,20,21,24,25,26,28:35
myresult<-glm(data=train,AttritionTarget ~ AGE+BusinessTravel+
                +DailyRate+Department+DistanceFromHome+Education+
                EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
                JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+      #all
                NumCompaniesWorked+OverTime+PercentSalaryHike+
                RelationshipSatisfaction+StandardHours+StockOptionLevel+
                TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family=binomial)

summary(myresult)

#Gives best fitted model
#To choose a good model

?step
reduced<-step(myresult,direction="backward")


# Iteration 2:    #retain acc. to step function
myresult<-glm(data=train,AttritionTarget ~ Age + BusinessTravel + DailyRate + Department + 
                DistanceFromHome + EnvironmentSatisfaction + Gender + JobInvolvement + 
                JobSatisfaction + MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
                OverTime + RelationshipSatisfaction + StockOptionLevel + 
                TrainingTimesLastYear + YearsInCurrentRole + YearsSinceLastPromotion + 
                YearsWithCurrManager,family=binomial)

summary(myresult)

myresult<-glm(data=train,AttritionTarget ~ AGE + BusinessTravel + DailyRate + Department + 
  DistanceFromHome + EnvironmentSatisfaction + Gender + JobInvolvement + 
  JobSatisfaction + MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
  OverTime + RelationshipSatisfaction + WorkLifeBalance + YearsAtCompany + 
  YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager,family =binomial)

summary(myresult)
# Creating dummy variables   #we are taking 3 variable frequently reraley those factor variable

train$BTF <- ifelse(train$BusinessTravel == "Travel_Frequently",1,0)
train$OTY <- ifelse(train$OverTime == "Yes",1,0)


test$BTF <- ifelse(test$BusinessTravel == "Travel_Frequently",1,0)
test$OTY <- ifelse(test$OverTime == "Yes",1,0)
str(train)
#Iteration # 3:  #retain significant variable


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome + NumCompaniesWorked + 
                OTY + YearsSinceLastPromotion,family=binomial)

summary(myresult)

?confusionMatrix
#performance to obtain prediction
pred<-predict(model)
# Iteration # 4


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome +  
                OTY,family=binomial)

summary(myresult)


#Finding Predicted Values

?glm

myresult$fitted.values   #gives me the probability


train$predicted <- myresult$fitted.values   #probabilty of attrition of 1
train$predicted

View(train)
# Compare with actual data

head(train$AttritionTarget)

head(train$predicted)   #0.20 % or 20% of attrition has happened

# Let us convert the probabilities also into Attrited/Not atttrited
# based on a cut-off probability

#Confusion Matrix
train$predclass<-ifelse(train$predicted>0.5,1,0)  #1,0 value instead of probability,,, probabilty is 
#more than 50% is 1 and less than 50% is 0
table(train$predclass,train$AttritionTarget)
#   0                                      1          (actual)
#0 859(true negative)             133(false negative/model predicted )
#1  11(false positive)                 26(true event/true positive)
dim(train)
#True Positive+ True Negative should be high. 

# Accuracy = (TP+TN)/(P+N)

(859+26)/(859+26+133+11)
859+26+133+11 =1029



# For different cutoff probabilities, the confusion matrix will be different

# To find accuracies for different cut-off probabilities

# There are a lot of performance parameters available in ROCR package

install.packages("ROCR")   #it provides all performance measures information aboout model
library(ROCR)


# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(train$predicted,train$AttritionTarget)

class(pred)      #ROCR :- Receiver operating charcteristics curve

?performance

perf <- performance(pred,"acc")
class(perf)
perf
# x values contain the cut-off probabilities

#use @ to access the slots
#extracting cutoff probabailty and
class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

cutoffs <- data.frame(cutoffprob, accuracies )
# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 

train$predclass <- ifelse(train$predicted>0.4418971,1,0)

# Kappa values and Confusion Matrix from caret package
install.packages("caret")
library(caret)
install.packages("irr")
library(irr)

kappa2(data.frame(train$AttritionTarget,train$predclass))
?kappa2    #kappa high then it is good

confusionMatrix(as.factor(train$AttritionTarget),as.factor(train$predclass), positive = "1")

install.packages("e1071")
## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline
# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred,"auc")
auc

#Creating a Gains chart

install.packages("gains")

library(gains)  # top probability (highest people we should target who leave the company)


gains(as.numeric(train$AttritionTarget),train$predicted, groups =10) #top 10%:- 32.7% of ones

#57% people leave out in 2nd decile(top 30%:- able to capture 70% of ones)
#seq(0,1,0.25):- 75% has the probabailty <=0.20 

#top 20%:- 57.7%  and from decile 80%:-probability is 0.241
quantile(train$predicted, seq(0,1,0.1))
  #if i pick the id above 70 who is leaving the company :- so we just want to prioratize(gains)
targeted <- which(train$predicted >=  0.369199242 )

targeted

# To obtain predictions from the model, use the predict() function.
install.packages("knitr")
library(knitr)
?predict()
test$pred <- predict(myresult, type = "response",newdata = test)  #applying the model0 


# The value 'response' to the parameter type would make sure 
# that these predictions are returned as probability of events.

