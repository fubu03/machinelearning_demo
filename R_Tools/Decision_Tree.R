#identifying risky bank loans
credit<-read.csv("credit.csv")
list.files()

str(credit)
View(credit)
table(credit$checking_balance)
table(credit$savings_balance)
table(credit$default)
#checking column position # practice R
table(credit[17])
which(colnames(credit) == "default" )
credit$default<-factor(credit$default,levels = c(1,2),labels = c("no","yes"))

#randomizing the data
set.seed(12345)
credit_rand<-credit[order(runif(1000)),]
head(credit$amount)
head(credit_rand$amount)
#dividing data ,90, 10 this time

credit_train<-NULL
credit_test<-NULL
credit_train<-credit_rand[1:round(nrow(credit)*0.9),]

credit_test<-credit_rand[c(round(nrow(credit)*0.9)+1):1000,]
View(credit_test)

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#training model on the data
install.packages("C50")
library(C50)
?C5.0()

#training the model

#need to remove 17th column i.e. default from credit

credit_model<-C5.0(credit_train[-17],credit_train$default,trials = 1)
credit_model
summary(credit_model)


#evaluating model

credit_predict<-predict(credit_model,credit_test)
?predict()
library(gmodels)
credit_predict
CrossTable(credit_test$default,credit_predict,prop.chisq = f,prop.c = F,prop.r = F,
           dnn=c('actual ','predicted '))

nrow(credit_test)
ncol(credit_test)
length(credit_test$default)
length(credit_predict[1:100])

#from confusion matrix
#specificity:
65/73


#senstivity:
11/19


#Improving performance of the table

credit_boost10<-C5.0(credit_train[-17],credit_train$default,trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10<-predict(credit_boost10,credit_test)

CrossTable(credit_test$default,credit_boost_pred10,prop.chisq = f,prop.c = F,prop.r = F,
           dnn=c('actual ','predicted '))



#using cost variable matrix
#improving efficiency
error_cost<-matrix(c(0,1,4,0),nrow = 2)


credit_cost<-C5.0(credit_train[-1],credit_train$default)
credit_cost_predict<-predict(credit_cost,credit_test,type = "prob")

library(gmodels)
CrossTable(credit_test$default,credit_cost_predict,
           prop.chisq = F,prop.r = F,prop.c = F,
           dnn = c('Actual','Predicted'))


View(credit_cost_predict)
length(credit_cost_predict)
str(credit_cost_predict)
a<-credit_test
a<-cbind(a,credit_cost_predict)
View(a)


### TUNING PERFORMANCE ###

#using caret package for automated parameter tuning

install.packages("caret")
library(caret)

# creating a simple tuned model

set.seed(300)
m<-train(default~.,data=credit,method="C5.0")
p<-predict(m,credit,type="prob")
p
summary(p)
table(p,credit$default)
