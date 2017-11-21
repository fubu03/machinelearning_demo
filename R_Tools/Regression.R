#Multiple Linear Regression
getwd()
ins<-read.csv("insurance.CSV",stringsAsFactors = T)
View(ins)

str(ins)

#dependent variable is charges

summary(ins$charges)

# since mean>median..means right skewed

#confirming visually

hist(ins$charges)

# not a normal distribution and we need one

# lets take a look at regions

table(ins$region)

#Exploring Relationships amongst independent variables
colnames(ins)
cor(ins[c("age" ,"bmi","children","charges")])

#always :: cor(y,x) = cor(x,y)

#visualizing relationships
#pairs() #splom - scatterplot matrix

#imiting data to 4 vARIABLES ONLY

pairs(ins[c("age" ,"bmi","children","charges")])

#adding more info to visualization
install.packages("psych")
library(psych)

pairs.panels(ins[c("age" ,"bmi","children","charges")])


# developing the model

ins_model<-lm(charges~age+children+bmi+sex+smoker+region,data=ins)
#OR
ins_model1<-lm(charges~.,data=ins)

ins_model



#evaluating model performance

summary(ins_model)

#improving model performance
ins$bmi30<-ifelse(ins$bmi >30 , 1,0)

#adding interaction effects
#adding combined effects of two x.
# charges~ bim30+smokeryes+bmi30:smokeryes
# this included bmi30, smoker and their interaction
# interaction = charges~bmi30*smokeryes



# an imoproved regression model:
ins_model2<-lm(charges~age+children+bmi+sex+bmi*smoker
               +region,data= ins)
summary(ins_model2)





