#reading wisconsin data
wbcd<-read.csv("wisc_bc_data.csv", stringsAsFactors = F)
str(wbcd)
View(wbcd)
#always drop ID variable as it may cause overfitting- won't generalize

wbcd<-wbcd[-1]

#output variable -> diagnosis . which we aim to predict
attach(wbcd)
table(wbcd$diagnosis)

#many algos need target feature to be factor - so re-recording it

wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Benign","Malignant"))

round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
wbcd["diagnosis"]


#normalizing data
normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
  
}
#testing normalize
normalize(c(1,2,3,4,5))

#instead of applying all 30 numeric data, we'll automate using lapply

wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
View(wbcd_n)

summary(wbcd_n$area_mean)

#splitting the data sets

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]


#creating data for evaluation with target feature-column 1
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]


#installing a R package - KNN
install.packages("class")
library(class)
?knn()

wbcd_pred<-knn(train = wbcd_train,test = wbcd_test,cl = wbcd_train_labels,k=21)

#evaluating 
library(gmodels)

CrossTable(x = wbcd_test_labels,y = wbcd_pred, prop.chisq = F)


# now trying to improve with Z -score standardization
scale()

wbcd_z<-as.data.frame(scale(wbcd[-1]))
View(wbcd_z)
summary(wbcd_z$area_mean)

#Again dividing data and performing calculations


wbcd_train <-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]


wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

wbcd_pred<-knn(train= wbcd_train,test = wbcd_test,cl = wbcd_train_labels,k=21)
CrossTable(x = wbcd_test_labels,y = wbcd_pred, prop.chisq = F)
